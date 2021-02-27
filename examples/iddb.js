// IdDB - DB with id primary key column in all tables
const { freeze: harden } = Object; // TODO: @agoric/harden
// TODO: @rchain-community/js2rho
import { tuple, Channel, RhoMap, RhoSet, quote, deref } from "./lib/rspace.js";

// TODO: import deployId from 'rho:rchain:deployId';
// import deployerId from 'rho:rchain:deployerId';
// import insertArbitrary from 'rho:registry:insertArbitrary';
import { deployId, deployerId } from "./lib/rchain.js";
import { insertArbitrary } from "./lib/registry.js";

const IdDB = harden({
  make() {
    const tablesCh = Channel(undefined);
    tablesCh.put(RhoMap()); // table_name -> channel with Set of keys
    const self = harden({
      // rows are stored at @{[self, table_name, key]}
      // TODO: consider TreeHashMap, since set of keys could get large
      create_table(table_name /*: String*/) {
        // TODO: fail if table_name already present
        return tablesCh.get().then((tables) => {
          const keysCh = Channel(undefined);
          keysCh.put(RhoSet());
          tablesCh.put(tables.set(table_name, keysCh));
          console.log({ created: table_name, with: tables });
          return null;
        });
      },

      // TODO: drop table: delete from map @tablesCh; consume keysCh, all rows

      keys(table_name /*: String*/) {
        return tablesCh.peek().then((tables) => {
          const keysCh = tables.get(table_name);
          if (null) {
            return tuple(false, "no such table");
          } else {
            return keysCh.peek().then((keys) => {
              console.log({ "got keys for": table_name, qty: keys.size() });
              return tuple(true, keys);
            });
          }
        });
      },

      // TODO: select method to get record from table_name, key
      // TODO: move keys, select to read-only facet

      INSERT(table_name /*: String*/, _OLD, NEW) {
        return tablesCh.peek().then((tables) => {
          const keysCh = tables.get(table_name);
          const key = NEW["id"];
          if (!keysCh) {
            return tuple(false, "no such table");
          } else if (!key) {
            return tuple(false, "NEW has no id key");
          } else {
            return keysCh.get().then((keys) => {
              if (keys.contains(key)) {
                keysCh(keys);
                return tuple(false, "key already present");
              } else {
                keysCh(keys.union(RhoSet(key)));
                quote([deref(self), table_name, key])(NEW);
                console.log({ "inserted key": key, with: keys });
                return tuple(true, key);
              }
            });
          }
        });
      },

      DELETE(table_name /*: String*/, OLD, _NEW) {
        return tablesCh.peek().then((tables) => {
          const keysCh = tables.get(table_name);
          const key = OLD["id"];
          if (!keysCh) {
            return tuple(false, "no such table");
          } else if (!key) {
            return tuple(false, "OLD has no id key");
          } else {
            return keysCh.peek().then((keys) => {
              if (!keys.contains(key)) {
                keysCh(keys);
                return tuple(false, "key not present");
              } else {
                return quote([deref(self), table_name, key])
                  .get()
                  .then(() => {
                    keysCh(keys.delete(key));
                    console.log({ "deleted key": key, from: keys });
                    return tuple(true, key);
                  });
              }
            });
          }
        });
      },

      UPDATE(table_name /*: String*/, OLD, NEW) {
        return tablesCh.peek().then((tables) => {
          const keysCh = tables.get(table_name);
          const key = OLD["id"];
          if (!keysCh) {
            return tuple(false, "no such table");
          } else if (!key) {
            return tuple(false, "OLD has no id key");
          } else {
            return keysCh.peek().then((keys) => {
              if (!keys.contains(key)) {
                return tuple(false, "key not present");
              } else {
                const loc = quote([deref(self), table_name, key]);
                return loc.get().then(() => {
                  loc(NEW);
                  console.log({ "updated key": key });
                  return tuple(true, key);
                });
              }
            });
          }
        });
      },
    });
    return self;
  },
});

export default async function main() {
  // register IdDB constructor contract
  const uri = await insertArbitrary(IdDB);
  console.log({ "IdDB URI": uri });
  deployId.put(uri);

  // testing
  const assert = (ok) => {
    if (!ok) {
      throw new Error("assertion failed");
    }
  };
  const db1 = await IdDB.make();
  console.log({ db1: deref(db1) });
  async function logKeys(label, key) {
    console.log({ [label]: key });
    const [ok, keys] = await db1.keys("player");
    assert(ok);
    console.log({ [label]: key, keys: keys });
  }
  await db1.create_table("player");
  console.log("create_table player done");
  const [ok, key] = await db1.INSERT("player", null, {
    id: 123,
    name: "Pete Rose",
    average: 400,
  });
  assert(ok);
  logKeys("inserted", key);
  const [ok2, key2] = await db1.UPDATE(
    "player",
    { id: 123 },
    { id: 123, name: "Pete Rose", average: 250 }
  );
  assert(ok2);
  logKeys("updated", key2);
  const [ok3, key3] = await db1.DELETE("player", { id: 123 }, null);
  assert(ok3);
  logKeys("deleted", key3);
}

main().catch((err) => console.error(err));
