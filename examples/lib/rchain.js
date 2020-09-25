const { freeze: harden } = Object;

import { Channel } from "./rspace.js";

export const deployId = Channel();
export const deployerId = Channel();
