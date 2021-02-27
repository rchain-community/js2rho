// @ts-check
import { js2rho } from "../lib/js2rho.js";

/**
 * @param {Rd} examples
 * @param {Wr} outDir
 * @typedef { ReturnType<typeof mkRd> } Rd
 * @typedef { ReturnType<typeof mkWr> } Wr
 */
async function run(examples, outDir) {
  const tests = await load(examples);
  console.log(Object.keys(tests));

  await outDir.mkdir({ recursive: true });
  for (const entry of Object.entries(tests)) {
    const [name, info] = entry;
    console.log("\n==== TEST CASE: ", name);
    if (!("input" in info)) {
      console.log("== no input");
      continue;
    }
    console.log(
      "js input:",
      info.input.content.length,
      info.input.content.slice(0, 60)
    );
    const actual = outDir.resolve(name + ".rho");
    await actual.withSyncWriter(async (ws) => {
      try {
        await js2rho(info.input.content, ws);
      } catch (compileError) {
        // console.warn(info.input.name, compileError);
        console.log("@@lose");
      }
    });
    console.log("\n==== DONE: ", name);
  }
  console.log("test suite run DONE.");
}

export default function unittest(out) {}

/**
 * @param {Rd} examples
 * @returns {Promise<Record<string, Case>>}
 * @typedef {{ input?: FileInfo, expected?: FileInfo }} Case
 * @typedef {{ name: string, content: string }} FileInfo
 */
async function load(examples) {
  const names = await examples.readdir();
  /** @type { Record<string, Case> } */
  const cases = {};
  for (const name of names) {
    if (!name.includes(".")) {
      continue;
    }
    const basename = name.replace(/\..*$/, "");
    if (!(basename in cases)) {
      cases[basename] = {};
    }
    const info = cases[basename];
    const content = await examples.resolve(name).readFile();
    if (name.endsWith(".js")) {
      info.input = { name, content };
    }
    if (name.endsWith(".rho")) {
      info.expected = { name, content };
    }
  }
  return cases;
}

/**
 * @param {typeof import('fs')} fs
 * @param {typeof import('fs').promises} fsp
 * @param {string} path
 */
function mkWr(fs, fsp, path) {
  return Object.freeze({
    /**
     * @param {(writer: SyncWriter) => Promise<void>} thunk
     * @typedef { { write: (data: string) => void } } SyncWriter
     */
    async withSyncWriter(thunk) {
      /** @type { string[] } */
      const buf = [];
      const writer = {
        /** @param { string } data */
        write(data) {
          buf.push(data);
        },
      };
      const ret = await thunk(writer);
      fs.writeFileSync(path, buf.join(""));
      return ret;
    },
    mkdir(options) {
      return fsp.mkdir(path, options);
    },
    /** @param { string } there */
    resolve(there) {
      return mkWr(fs, fsp, `${path}/${there}`);
    },
    readOnly() {
      return mkRd(fsp, path);
    },
  });
}

/**
 * @param {typeof import('fs').promises} fsp
 * @param {string} path
 */
function mkRd(fsp, path) {
  return Object.freeze({
    async readFile() {
      return fsp.readFile(path, { encoding: "utf-8" });
    },
    async readdir() {
      return fsp.readdir(path);
    },
    resolve(there) {
      return mkRd(fsp, `${path}/${there}`);
    },
  });
}

(({ fs }) => {
  run(mkRd(fs.promises, "examples"), mkWr(fs, fs.promises, "test-results"))
    .then((_) => console.log("== run DONE."))
    .catch((crash) => {
      console.log(crash);
    });
})({ fs: require("fs") });
