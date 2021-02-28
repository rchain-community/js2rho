// @ts-check
import harden from "@agoric/harden";

const makeCounter = harden({
  make() {
    let count = 0;
    const self = harden({
      incr() {
        count = count + 1;
        return count;
      },
      decr() {
        count = count - 1;
        return count;
      },
    });
    return self;
  },
});
