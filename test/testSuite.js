// @ts-check
import * as fs from 'fs';
import assert from 'assert';

import { js2rho } from '../lib/js2rho.js';

async function run(examples, out) {
    const tests = await load(examples);
    console.log(Object.keys(tests));

    for (const entry of Object.entries(tests)) {
        const [name, info] = entry;
        console.log('\n==== TEST CASE: ', name);
        if (!('input' in info)) {
            console.log('== no input');
            continue;
        }
        console.log('js input:', info.input.content.length, info.input.content.slice(0, 60));
        js2rho(info.input.content, out);
    }
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

run(mkRd(fs.promises, 'examples'), process.stdout);
