import { WebR, ChannelType } from "https://webr.r-wasm.org/v0.6.0/webr.mjs";

const R_PACKAGES = ["dplyr", "tidyr", "readr", "stringr", "purrr", "lubridate", "jsonlite"];
const LOG_FILES = [
  {
    virtualPath: "/tmp/browser_output/logs/log_long_events_3h_plus.csv",
    destination: ["logs", "log_long_events_3h_plus.csv"],
  },
  {
    virtualPath: "/tmp/browser_output/logs/additional_logs/log_bad_apps_truncated_rows.csv",
    destination: ["logs", "additional_logs", "log_bad_apps_truncated_rows.csv"],
  },
  {
    virtualPath: "/tmp/browser_output/logs/additional_logs/log_bad_apps_truncated_counts.csv",
    destination: ["logs", "additional_logs", "log_bad_apps_truncated_counts.csv"],
  },
  {
    virtualPath: "/tmp/browser_output/logs/additional_logs/log_gaps_over_12h.csv",
    destination: ["logs", "additional_logs", "log_gaps_over_12h.csv"],
  },
  {
    virtualPath: "/tmp/browser_output/logs/additional_logs/log_partial_days_flagged_per_participant.csv",
    destination: ["logs", "additional_logs", "log_partial_days_flagged_per_participant.csv"],
  },
  {
    virtualPath: "/tmp/browser_output/logs/additional_logs/log_partial_days_flagged_dates.csv",
    destination: ["logs", "additional_logs", "log_partial_days_flagged_dates.csv"],
  },
];

const elements = {
  deviceType: document.querySelector("#deviceType"),
  customBadApps: document.querySelector("#customBadApps"),
  selectInputFolder: document.querySelector("#selectInputFolder"),
  inputFolderName: document.querySelector("#inputFolderName"),
  selectOutputFolder: document.querySelector("#selectOutputFolder"),
  outputFolderName: document.querySelector("#outputFolderName"),
  durationTypes: document.querySelector("#durationTypes"),
  collapseTypes: document.querySelector("#collapseTypes"),
  sessionGapSecs: document.querySelector("#sessionGapSecs"),
  maxBadAppMins: document.querySelector("#maxBadAppMins"),
  long3hHours: document.querySelector("#long3hHours"),
  long6hHours: document.querySelector("#long6hHours"),
  longGapHours: document.querySelector("#longGapHours"),
  longEventAction: document.querySelector("#longEventAction"),
  longEventScope: document.querySelector("#longEventScope"),
  runButton: document.querySelector("#runButton"),
  clearLog: document.querySelector("#clearLog"),
  log: document.querySelector("#log"),
  statusBadge: document.querySelector("#statusBadge"),
};

const radioButtons = [...document.querySelectorAll('input[name="badAppsSource"]')];
const disabledWhileRunning = [
  elements.deviceType,
  elements.customBadApps,
  elements.selectInputFolder,
  elements.selectOutputFolder,
  elements.durationTypes,
  elements.collapseTypes,
  elements.sessionGapSecs,
  elements.maxBadAppMins,
  elements.long3hHours,
  elements.long6hHours,
  elements.longGapHours,
  elements.longEventAction,
  elements.longEventScope,
  ...radioButtons,
];

let webR = null;
let webRReady = false;
let running = false;
let inputDirectory = null;
let outputDirectory = null;

function log(message = "") {
  const timestamp = new Date().toLocaleTimeString();
  const nextLine = `[${timestamp}] ${message}`;
  elements.log.textContent = elements.log.textContent.trim()
    ? `${elements.log.textContent.trim()}\n${nextLine}`
    : nextLine;
  elements.log.scrollTop = elements.log.scrollHeight;
}

function setStatus(text, className) {
  elements.statusBadge.textContent = text;
  elements.statusBadge.className = `status-badge ${className}`;
}

function friendlyError(error) {
  if (error?.name === "AbortError") return "Folder selection was cancelled.";
  if (error?.name === "NotAllowedError") {
    return "Chrome or Edge did not grant the required permission for the selected folder.";
  }

  return String(error?.message || error)
    .replace(/^Error:\s*/i, "")
    .replace(/^evaluation error:\s*/i, "")
    .trim();
}

function selectedBadAppsSource() {
  return radioButtons.find((input) => input.checked)?.value || "repository";
}

function updateControls() {
  const customSelected = selectedBadAppsSource() === "custom";
  const canRun = webRReady && inputDirectory && outputDirectory && !running;

  elements.runButton.disabled = !canRun;
  elements.customBadApps.disabled = running || !customSelected;
  for (const control of disabledWhileRunning) {
    if (control !== elements.customBadApps) control.disabled = running;
  }
}

function formatMegabytes(bytes) {
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

function formatGigabytes(bytes) {
  return `${(bytes / (1024 * 1024 * 1024)).toFixed(2)} GB`;
}

function parseInteractionTypes(value, label) {
  const values = value
    .split(",")
    .map((item) => item.trim())
    .filter(Boolean);
  if (values.length === 0) throw new Error(`${label} must contain at least one value.`);
  return [...new Set(values)];
}

function positiveNumber(element, label, { allowZero = false } = {}) {
  const value = Number(element.value);
  const valid = Number.isFinite(value) && (allowZero ? value >= 0 : value > 0);
  if (!valid) throw new Error(`${label} must be ${allowZero ? "zero or greater" : "greater than zero"}.`);
  return value;
}

function getConfig() {
  const config = {
    device_type: elements.deviceType.value,
    duration_interaction_types: parseInteractionTypes(
      elements.durationTypes.value,
      "Duration-bearing interaction types"
    ),
    collapse_interaction_types: parseInteractionTypes(
      elements.collapseTypes.value,
      "Collapse interaction types"
    ),
    max_bad_app_secs: positiveNumber(elements.maxBadAppMins, "Bad-app maximum") * 60,
    long_3h_secs: positiveNumber(elements.long3hHours, "Long-event review threshold") * 60 * 60,
    long_6h_secs: positiveNumber(elements.long6hHours, "Long-event action threshold") * 60 * 60,
    long_gap_hours: positiveNumber(elements.longGapHours, "Long data-gap threshold"),
    session_gap_secs: positiveNumber(elements.sessionGapSecs, "Session gap", { allowZero: true }),
    long_event_action: elements.longEventAction.value,
    apply_long_event_action_to: elements.longEventScope.value,
  };

  if (config.long_6h_secs < config.long_3h_secs) {
    throw new Error("The long-event action threshold cannot be lower than the review threshold.");
  }
  return config;
}

async function verifyPermission(handle, readWrite = false) {
  const options = readWrite ? { mode: "readwrite" } : { mode: "read" };
  if ((await handle.queryPermission(options)) === "granted") return true;
  return (await handle.requestPermission(options)) === "granted";
}

async function chooseDirectory(id, readWrite = false) {
  if (!("showDirectoryPicker" in window)) {
    throw new Error("This browser cannot select local folders. Use a current version of Chrome or Edge over HTTPS.");
  }
  const handle = await window.showDirectoryPicker({
    id,
    mode: readWrite ? "readwrite" : "read",
    startIn: "documents",
  });
  if (!(await verifyPermission(handle, readWrite))) {
    throw new Error(`The selected ${readWrite ? "output" : "input"} folder was not granted ${readWrite ? "read/write" : "read"} permission.`);
  }
  return handle;
}

async function directoryHasContents(directory) {
  for await (const _entry of directory.values()) return true;
  return false;
}

async function collectCsvFiles(directory, relativeParts = []) {
  const entries = [];
  for await (const [name, handle] of directory.entries()) entries.push({ name, handle });
  entries.sort((a, b) => a.name.localeCompare(b.name));

  const files = [];
  for (const entry of entries) {
    if (entry.handle.kind === "directory") {
      files.push(...(await collectCsvFiles(entry.handle, [...relativeParts, entry.name])));
    } else if (/\.csv$/i.test(entry.name)) {
      files.push({
        name: entry.name,
        handle: entry.handle,
        relativePath: [...relativeParts, entry.name].join("/"),
      });
    }
  }
  return files;
}

function assertSafeOutputName(name) {
  if (
    typeof name !== "string" ||
    !/^[^\\/\0]+_cleaned\.csv$/i.test(name) ||
    name === "." ||
    name === ".."
  ) {
    throw new Error("The cleaner returned an unsafe output filename. No files were written.");
  }
}

async function writeFile(directory, name, data) {
  const handle = await directory.getFileHandle(name, { create: true });
  const writable = await handle.createWritable({ keepExistingData: false });
  try {
    await writable.write(data);
    await writable.close();
  } catch (error) {
    try {
      await writable.abort();
    } catch {
      // The stream may already be closed.
    }
    throw error;
  }
}

async function appendFile(directory, name, data) {
  let handle;
  try {
    handle = await directory.getFileHandle(name, { create: false });
  } catch (error) {
    if (error.name !== "NotFoundError") throw error;
    await writeFile(directory, name, data);
    return;
  }

  const existingSize = (await handle.getFile()).size;
  const writable = await handle.createWritable({ keepExistingData: true });
  try {
    await writable.write({ type: "write", position: existingSize, data });
    await writable.close();
  } catch (error) {
    try {
      await writable.abort();
    } catch {
      // The stream may already be closed.
    }
    throw error;
  }
}

class OutputTransaction {
  constructor(root) {
    this.root = root;
    this.started = false;
    this.createdTopLevel = new Set();
  }

  async start() {
    if (await directoryHasContents(this.root)) {
      throw new Error("The selected output folder already contains a file or subfolder. Choose an empty folder before running.");
    }
    this.started = true;
    await this.getDirectory(["logs", "additional_logs"]);
  }

  async getDirectory(parts) {
    let current = this.root;
    for (let index = 0; index < parts.length; index += 1) {
      const name = parts[index];
      let created = false;
      try {
        current = await current.getDirectoryHandle(name, { create: false });
      } catch (error) {
        if (error.name !== "NotFoundError") throw error;
        current = await current.getDirectoryHandle(name, { create: true });
        created = true;
      }
      if (created && index === 0) this.createdTopLevel.add(name);
    }
    return current;
  }

  async write(destination, data) {
    const parts = [...destination];
    const filename = parts.pop();
    const directory = await this.getDirectory(parts);
    if (parts.length === 0) this.createdTopLevel.add(filename);
    await writeFile(directory, filename, data);
  }

  async appendCsv(destination, bytes) {
    const text = new TextDecoder().decode(bytes);
    const firstLineEnd = text.search(/\r?\n/);
    if (firstLineEnd < 0) return;

    const body = text.slice(firstLineEnd).replace(/^\r?\n/, "");
    if (!body.trim()) return;

    const parts = [...destination];
    const filename = parts.pop();
    const directory = await this.getDirectory(parts);
    let existing = false;
    try {
      await directory.getFileHandle(filename, { create: false });
      existing = true;
    } catch (error) {
      if (error.name !== "NotFoundError") throw error;
    }

    if (existing) {
      await appendFile(directory, filename, body);
    } else {
      await writeFile(directory, filename, text);
    }
  }

  async rollback() {
    for (const name of [...this.createdTopLevel].reverse()) {
      try {
        await this.root.removeEntry(name, { recursive: true });
      } catch {
        // Keep cleaning up the remaining output files where possible.
      }
    }
  }
}

async function removeWebRFile(path) {
  try {
    await webR.FS.unlink(path);
  } catch {
    // It may not have been created yet.
  }
}

async function writeBytesToWebR(path, bytes) {
  await removeWebRFile(path);
  await webR.FS.writeFile(path, bytes);
}

async function writeHandleToWebR(path, fileHandle, description) {
  const file = await fileHandle.getFile();
  if (file.size === 0) throw new Error(`${description} is empty.`);

  const chunkSize = 8 * 1024 * 1024; // 8 MB
  await removeWebRFile(path);

  for (let offset = 0; offset < file.size; offset += chunkSize) {
    const chunk = new Uint8Array(
      await file
        .slice(offset, Math.min(offset + chunkSize, file.size))
        .arrayBuffer()
    );

    await webR.FS.writeFile(path, chunk, offset === 0 ? "w" : "a");
  }

  return file.size;
}

async function readWebRFile(path) {
  const contents = await webR.FS.readFile(path);
  return contents instanceof Uint8Array ? contents : new Uint8Array(contents);
}

async function readWebRFileIfPresent(path) {
  try {
    return await readWebRFile(path);
  } catch {
    return null;
  }
}

async function evaluateSummary() {
  const result = await webR.evalR("browser_last_result()");
  try {
    const rows = await result.toD3();
    if (!Array.isArray(rows) || rows.length !== 1) {
      throw new Error("R returned an unexpected processing summary.");
    }
    return rows[0];
  } finally {
    await webR.destroy(result);
  }
}

async function getBadAppsBytes() {
  if (selectedBadAppsSource() === "custom") {
    const file = elements.customBadApps.files?.[0];
    if (!file) throw new Error("Select the custom bad-app CSV, or switch back to the repository list.");
    if (file.size === 0) throw new Error("The custom bad-app CSV is empty.");
    return new Uint8Array(await file.arrayBuffer());
  }

  const response = await fetch(new URL("../bad_apps.csv", import.meta.url), { cache: "no-store" });
  if (!response.ok) throw new Error(`Could not load the repository bad-app list (${response.status}).`);
  return new Uint8Array(await response.arrayBuffer());
}

async function initialise() {
  try {
    setStatus("Loading R…", "status-loading");
    log("Starting R in the browser.");
    webR = new WebR({ channelType: ChannelType.PostMessage });
    await webR.init();

    log("Loading the R packages needed for this pipeline. This is required only when the page first loads.");
    await webR.installPackages(R_PACKAGES);

    const response = await fetch("web/analysis.R", { cache: "no-store" });
    if (!response.ok) throw new Error(`Could not load web/analysis.R (${response.status}).`);
    await webR.evalRVoid(await response.text());

    webRReady = true;
    setStatus("Ready", "status-ready");
    log("R is ready. Select the input and output folders.");
  } catch (error) {
    setStatus("R failed to load", "status-error");
    log(`ERROR: ${friendlyError(error)}`);
  } finally {
    updateControls();
  }
}

async function prepareRun() {
  if (!(await verifyPermission(inputDirectory, false))) {
    throw new Error("Read permission for the selected input folder is no longer available.");
  }
  if (!(await verifyPermission(outputDirectory, true))) {
    throw new Error("Read/write permission for the selected output folder is no longer available.");
  }
  if (typeof inputDirectory.isSameEntry === "function" && await inputDirectory.isSameEntry(outputDirectory)) {
    throw new Error("The input and output folders must be different folders.");
  }
  if (await directoryHasContents(outputDirectory)) {
    throw new Error("The selected output folder already contains a file or subfolder. Choose an empty folder before running.");
  }

  const config = getConfig();
  const sourceFiles = await collectCsvFiles(inputDirectory);
  if (sourceFiles.length === 0) throw new Error("No CSV files were found in the selected input folder or its subfolders.");

  let totalBytes = 0;
  let largestFile = { name: "", bytes: 0 };
  for (const source of sourceFiles) {
    const file = await source.handle.getFile();
    totalBytes += file.size;
    if (file.size > largestFile.bytes) largestFile = { name: source.relativePath, bytes: file.size };
  }

  const badAppsBytes = await getBadAppsBytes();
  log(`Found ${sourceFiles.length} source CSV file(s): ${formatGigabytes(totalBytes)} total.`);
  log(`Largest file: ${largestFile.relativePath || largestFile.name} (${formatMegabytes(largestFile.bytes)}). Files are processed one at a time.`);
  return { config, sourceFiles, badAppsBytes, totalBytes };
}

async function processFile(source, config, badAppsBytes, transaction, usedOutputNames) {
  await webR.evalRVoid("reset_browser_workspace()");

  const fileSize = await writeHandleToWebR(
    "/tmp/browser_input.csv",
    source.handle,
    source.relativePath
  );

  await writeBytesToWebR("/tmp/browser_bad_apps.csv", badAppsBytes);
  await writeBytesToWebR(
    "/tmp/browser_config.json",
    new TextEncoder().encode(JSON.stringify(config))
  );

  await webR.evalRVoid(
    "clean_browser_file('/tmp/browser_input.csv', '/tmp/browser_output', '/tmp/browser_bad_apps.csv', '/tmp/browser_config.json')"
  );

  const summary = await evaluateSummary();
  const outputName = String(summary.output_file || "");
  assertSafeOutputName(outputName);

  if (usedOutputNames.has(outputName.toLowerCase())) {
    throw new Error(`Two input files would create '${outputName}'. No output was retained.`);
  }

  const cleanedBytes = await readWebRFile(`/tmp/browser_output/${outputName}`);
  await transaction.write([outputName], cleanedBytes);
  usedOutputNames.add(outputName.toLowerCase());

  for (const logFile of LOG_FILES) {
    const bytes = await readWebRFileIfPresent(logFile.virtualPath);
    if (bytes) await transaction.appendCsv(logFile.destination, bytes);
  }

  return {
    ...summary,
    input_bytes: fileSize,
  };
}

async function runApp() {
  if (running) return;
  running = true;
  updateControls();
  setStatus("Validating…", "status-running");
  let transaction = null;

  try {
    log("Beginning validation. No output has been created.");
    const prepared = await prepareRun();
    transaction = new OutputTransaction(outputDirectory);
    await transaction.start();

    log("All checks passed. Cleaning files one at a time.");
    setStatus("Cleaning…", "status-running");
    const usedOutputNames = new Set();
    let rowsRead = 0;
    let rowsWritten = 0;
    let longEvents = 0;
    let badAppTruncations = 0;

    for (let index = 0; index < prepared.sourceFiles.length; index += 1) {
      const source = prepared.sourceFiles[index];
      log(`Processing ${index + 1} of ${prepared.sourceFiles.length}: ${source.relativePath}`);
      const summary = await processFile(
        source,
        prepared.config,
        prepared.badAppsBytes,
        transaction,
        usedOutputNames
      );
      rowsRead += Number(summary.rows_read || 0);
      rowsWritten += Number(summary.rows_written || 0);
      longEvents += Number(summary.long_events || 0);
      badAppTruncations += Number(summary.bad_app_truncations || 0);
      log(
        `Completed ${summary.output_file}: ${summary.rows_written} output row(s), ${summary.long_events} long event(s), ${summary.bad_app_truncations} bad-app truncation(s).`
      );
    }

    setStatus("Complete", "status-success");
    log(
      `SUCCESS: ${prepared.sourceFiles.length} CSV file(s) cleaned. ${rowsRead} row(s) read; ${rowsWritten} row(s) written; ${longEvents} long event(s); ${badAppTruncations} bad-app truncation(s).`
    );
  } catch (error) {
    if (transaction?.started) {
      log("A processing error occurred. Removing files created during this run.");
      await transaction.rollback();
    }
    setStatus("Stopped", "status-error");
    log(`STOPPED: ${friendlyError(error)}`);
  } finally {
    try {
      if (webR) await webR.evalRVoid("reset_browser_workspace()");
    } catch {
      // Cleanup is best effort only.
    }
    running = false;
    updateControls();
  }
}

elements.selectInputFolder.addEventListener("click", async () => {
  try {
    inputDirectory = await chooseDirectory("chronicle-cleaning-input", false);
    elements.inputFolderName.textContent = inputDirectory.name;
    log(`Selected input folder: ${inputDirectory.name}`);
    if (webRReady) setStatus("Ready", "status-ready");
  } catch (error) {
    if (error?.name !== "AbortError") {
      setStatus("Input not selected", "status-error");
      log(`ERROR: ${friendlyError(error)}`);
    }
  } finally {
    updateControls();
  }
});

elements.selectOutputFolder.addEventListener("click", async () => {
  try {
    outputDirectory = await chooseDirectory("chronicle-cleaning-output", true);
    elements.outputFolderName.textContent = outputDirectory.name;
    log(`Selected output folder: ${outputDirectory.name}`);
    if (webRReady) setStatus("Ready", "status-ready");
  } catch (error) {
    if (error?.name !== "AbortError") {
      setStatus("Output not selected", "status-error");
      log(`ERROR: ${friendlyError(error)}`);
    }
  } finally {
    updateControls();
  }
});

for (const radio of radioButtons) {
  radio.addEventListener("change", updateControls);
}
elements.runButton.addEventListener("click", runApp);
elements.clearLog.addEventListener("click", () => {
  elements.log.textContent = "";
});

initialise();
