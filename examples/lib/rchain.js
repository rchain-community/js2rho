const { freeze: harden } = Object;

import { Channel } from "./rspace.js";

export const deployId = Channel(undefined);
export const deployerId = Channel(undefined);
