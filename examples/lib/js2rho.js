export const harden = (x) => Object.freeze(x);

export function tuple(...xs) {
  return harden(xs);
}

export function bundlePlus(proc) {
  return harden(proc);
}

export function RhoList(...items) {
  return harden(items); // different methods?
}

function makeMap(c0, ...c1n) {
  const m = new Map(c0);
  for (const col of c1n) {
    for (const [k, v] of col) {
      m.set(k, v);
    }
  }

  let _size = 0;
  for (const _ of m.values()) {
    _size++;
  }

  return harden({
    _size,
    size: () => _size,
    contains: (k) => m.has(k),
    get: (k) => m.get(k),
    set: (k, v) => makeMap(m.entries(), [[k, v]]),
    union: (m2) => makeMap(m.entries(), m2._entries()),
    _entries: () => m.entries(),
  });
}
export const RhoMap = (...entries) => makeMap(entries);

function* filter(predicate, items) {
  for (const item of items) {
    if (predicate(item)) {
      yield item;
    }
  }
}

function makeSet(c0, ...c1n) {
  const s = new Set(c0);
  for (const col of c1n) {
    for (const item of col) {
      s.add(item);
    }
  }

  let _size = 0;
  for (const _ of s.values()) {
    _size++;
  }

  return harden({
    _size,
    size: () => _size,
    contains: (k) => s.has(k),
    union: (s2) => makeSet(s.values(), s2._values()),
    delete: (k) => makeSet(filter((item) => item !== k, s.values())),
    _values: () => s.values(),
  });
}

export const RhoSet = (...items) => makeSet(items);

const proc2chan = new Map(); // non-Weak map to support structure sharing in quote()
const chan2proc = new WeakMap();

let id = 0;

export function Channel(proc) {
  id++;
  if (!proc) {
    const me = id;
    proc = harden({
      toByteArray() {
        return Buffer.from([1, 2, 3, me]);
      },
      toString: () => `Unf(${me})`,
    });
  }
  if (proc2chan.has(proc)) {
    return proc2chan.get(proc);
  }
  // console.log('new channel', { id });
  proc2chan.set(proc, push);
  chan2proc.set(push, proc);

  const q = [];
  const getters = [];
  const peekers = [];

  function push(proc) {
    while (peekers.length) {
      peekers.shift()(proc);
    }
    if (getters.length) {
      // console.log('push HIT', { id, proc, qlen: q.length });
      getters.shift()(proc);
    } else {
      // console.log('push miss', { id, proc, qlen: q.length });
      q.push(proc);
    }
  }

  push.get = () =>
    new Promise((resolve, _reject) => {
      if (q.length) {
        // console.log('get HIT', id);
        resolve(q.shift());
      } else {
        // console.log('get miss', id);
        getters.push(resolve);
      }
    });

  push.peek = () =>
    new Promise((resolve, _reject) => {
      if (q.length) {
        // console.log('peek HIT', id);
        resolve(q[0]);
      } else {
        // console.log('peek miss', id);
        peekers.push(resolve);
      }
    });

  push.id = id;
  push.toString = () => `@{${proc}})`;

  return harden(push);
}

const zip = (...rows) => [...rows[0]].map((_, c) => rows.map((row) => row[c]));

function listEq(a, b) {
  if (!Array.isArray(b)) {
    return false;
  }
  for (const [ia, ib] of zip(a, b)) {
    if (ia !== ib) {
      return false;
    }
  }
  return true;
}

export function quote(proc) {
  if (Array.isArray(proc)) {
    for (const [p, ch] of proc2chan.entries()) {
      if (listEq(proc, p)) {
        return ch;
      }
    }
  }
  const chan = proc2chan.has(proc) ? proc2chan.get(proc) : Channel(proc);
  // console.log('quote', { proc, chan });
  return chan;
}

export function deref(name) {
  if (chan2proc.has(name)) {
    const proc = chan2proc.get(name);
    // console.log('deref hit:', { id: name.id, proc });
    return proc;
  } else {
    const name2 = Channel(name);
    const proc = deref(name2);
    chan2proc.set(name, proc);
    // console.log('deref miss:', { name, id: name.id, id2: name2.id });
    return proc;
  }
}
