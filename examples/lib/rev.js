export const harden = x => Object.freeze(x);

let ix = 1;

export const RevAddress = harden({
    async fromUnforgeable(u) {
        ix += 1;
        return `111ABC${ix}`
    },
});

export const RevVault = harden({
    async findOrCreate(addr) {
        return harden({
            toString: () => `<rev vault @${addr}>`,
        });
    }
});
