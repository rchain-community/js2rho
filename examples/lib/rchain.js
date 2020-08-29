const harden = x => Object.freeze(x);

import { Channel } from './js2rho.js';

export const deployId = Channel();
export const deployerId = Channel();
