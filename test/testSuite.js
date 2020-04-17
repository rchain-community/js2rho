// @ts-check
import * as fs from 'fs';
import assert from 'assert';

import './esprimaGlobal.js';
import { js2rho } from '../lib/js2rho.js';

async function run(examples, outDir) {
    const tests = await load(examples);
    console.log(Object.keys(tests));

    await outDir.mkdir({ recursive: true });
    for (const entry of Object.entries(tests)) {
        const [name, info] = entry;
        console.log('\n==== TEST CASE: ', name);
        if (!('input' in info)) {
            console.log('== no input');
            continue;
        }
        console.log('js input:', info.input.content.length, info.input.content.slice(0, 60));
        const actual = outDir.resolve(name + '.rho');
        actual.withSyncWriter(ws => js2rho(info.input.content, ws));
        console.log('\n==== DONE: ', name);
    }
    console.log('test suite run DONE.');
}

export default
    function unittest(out) {
}


async function load(examples) {
    const names = await examples.readdir();
    const cases = {};
    for (const name of names) {
        const basename = name.replace(/\..*$/, '');
        if (!(basename in cases)) {
            cases[basename] = {};
        }
        const info = cases[basename];
        const content = await examples.resolve(name).readFile();
        if (name.endsWith('.js')) {
            info.input = { name, content };
        }
        if (name.endsWith('.rho')) {
            info.expected = { name, content };
        }
    }
    return cases;
}

function mkWr(fs, fsp, path) {
    return Object.freeze({
        withSyncWriter(thunk) {
            const buf = [];
            const writer = { write(data) { buf.push(data); } };
            let ret;
            ret = thunk(writer);
            fs.writeFileSync(path, buf.join(''));
            return ret;
        },
        mkdir(options) {
            return fsp.mkdir(path, options);
        },
        resolve(there) {
            return mkWr(fs, fsp, `${path}/${there}`);
        },
        readOnly() {
            return mkRd(fsp, path);
        }
    })
}

function mkRd(fsp, path) {
    return Object.freeze({
        async readFile() {
            return fsp.readFile(path, { encoding: 'utf-8' });
        },
        async readdir() {
            return fsp.readdir(path);
        },
        resolve(there) {
            return mkRd(fsp, `${path}/${there}`);
        }
    })
}


run(mkRd(fs.promises, 'examples'), mkWr(fs, fs.promises, 'test-results'))
    .then(_ => console.log('== run DONE.'))
    .catch(crash => console.error(crash));
