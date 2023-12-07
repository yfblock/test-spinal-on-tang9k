#!/usr/bin/node

import process from "node:process"
import fs from "node:fs"
import util from "node:util"
import child_process from "node:child_process"

function initSpiFlashData() {
    const fileBytes = [];
    for (let i = 1; i <= 256; i += 1) {
        fileBytes.push(i);
    }

    fs.writeFileSync('numbers.bin', Buffer.from(fileBytes));
}

async function runMain() {
    initSpiFlashData();
    child_process.spawnSync(
        "openFPGALoader",
        ["-b", "tangnano9k", "--external-flash", "numbers.bin"],
        {
            shell: true,
            stdio: 'inherit'
        });
    child_process.spawn("rm", ["numbers.bin"])
}

runMain()
