
// These are global variables mutated/queried by query execution
let environmentAll: Profile[]; // All the profiles
let environmentThis: Profile; // The specific profile under test
let environmentGroup: string[]; // The group produced as a result

function group(x: string): boolean {
    environmentGroup.push(x);
    return true;
}

function leaf(): boolean {
    return environmentThis.writers.length === 0;
}

function run(): number;
function run(i: timestamp): boolean;
function run(i?: timestamp): number | boolean {
    if (i === undefined)
        return environmentThis.built;
    else
        return environmentThis.built === i;
}

function named(): string;
function named(r: string | RegExp, groupName?: string): boolean;
function /* export */ named(r?: string | RegExp, groupName?: string): string | boolean {
    if (r === undefined)
        return environmentThis.name;

    const res = execRegExp(r, environmentThis.name);
    if (res === null) {
        if (groupName === undefined)
            return false;
        else {
            group(groupName);
            return true;
        }
    }
    if (res.length !== 1) {
        for (let i = 1; i < res.length; i++)
            group(res[i]);
    }
    return true;
}
