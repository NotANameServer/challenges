// ==========================================
//          BY TrétinV3#7056
// ==========================================

// v2.1.0

/*\
 |
 |  How to use ?
 |
 |  Juste run `node simpleIrcClient.js` and follow instructions
 |  /!\ A json file "db.json" will be create for save your servers /!\
 |
 |  Any bug ? -> Discord TrétinV3#7056 or Github https://github.com/TretinV3/SimpleIRC
 |
 |  Thanks NotAName for the idea (https://discord.gg/NotAName)
 |
\*/

const net = require('net');
const readline = require('readline');
const fs = require("fs");

let client = new net.Socket();
const rl = readline.createInterface({
    input: process.stdin,
    output: null,
    terminal: true,
    prompt: "",
});

let ip, port, name, id;

const clientState = {
    home: 0,
    pseudo: 1,
    server: 2,
    createServer: 3,
    password: 4,
    run: 10,
    quit: -1,
}

let state = clientState.home;

start();

function start() {
    clear();
    console.log(
        "\t┌────────────────────────┐\n" +
        "\t│                        │\n" +
        "\t│    SIMPLE IRC CLIENT   │\n" +
        "\t│                        │\n" +
        "\t│            by TretinV3 │\n" +
        "\t└────────────────────────┘\n" +
        "\n\n\t\tpress [ENTER]\n\n\n\n"
    )
    state = clientState.home;
}

function choseUser(pseudo) {
    if (!pseudo) {
        state = clientState.pseudo;
        clear();
        console.log(
            "Chose your username :\n" +
            "\n\n\n\n\n\n"
        )
    } else {
        name = pseudo;
        choseServeur();
    }
}

async function choseServeur(input) {

    if (!await fileExists("./db.json")) {
        await fs.writeFileSync('./db.json', JSON.stringify({ servers: [] }));
    }
    const db = JSON.parse(fs.readFileSync("./db.json"));
    state = clientState.server;
    if (!input
        || isNaN(Number(input))
        || Number(input) > db.servers.length
        || Number(input) < (db.servers.length * -1)
    ) {
        clear();
        console.log(
            "Chose the IRC server to connect :\n"
        );
        for (let i = 0; i < db.servers.length; i++) {
            console.log(`(${i + 1}) ${db.servers[i]}`);
        }
        console.log(
            "\n" +
            "Type 0 for add a new IRC server and add a \"-\" for remove it\n" +
            "\n\n\n\n\n"
        )
    } else {
        input = Number(input);
        if (input == 0) {
            addServeur();
        } else if (input > 0) {
            id = input;
            setPassword(true);
        } else if (input < 0) {
            db.servers.splice((input * -1) - 1, 1)
            fs.writeFileSync('./db.json', JSON.stringify(db, null, 2))
            choseServeur();
        }
    }
}

function setPassword(init, pass) {
    state = clientState.password;
    if (init) {
        clear();
        console.log(
            "Type your password (leave blank if you haven't)\n\n\n\n\n\n"
        )
    } else {
        selectServeur(id, pass);
    }
}

const fileExists = async path => !!(await fs.promises.stat(path).catch(e => false));

async function selectServeur(i, pass) {

    const db = JSON.parse(fs.readFileSync("./db.json"));
    port = db.servers[i - 1].split(":")[1];
    ip = db.servers[i - 1].split(":")[0];
    clear();

    client.connect(port, ip, () => {
        state = clientState.run;
        console.log('[clientIRC] Connected to server');
        console.log("[clientIRC] Try to log in...")
        if (pass) client.write(`PASS ${pass}\r\n`);
        client.write(`NICK ${name}\r\n`);
        client.write(`USER ${name} foo bar :foo bar\r\n`);
    });
}

client.on('error', e => {
    if (state !== clientState.quit) {
        if (e.code == "ECONNREFUSED") {
            console.log('[clientIRC] Could not connect to server...')
        } else {
            console.log('[clientIRC] An error occurred...')
        }
        setTimeout(() => {
            choseServeur();
        }, 1000);
    }
});

function addServeur(input) {
    state = clientState.createServer;
    const db = JSON.parse(fs.readFileSync("./db.json"));
    if (!input) {
        clear();
        console.log(
            "Type the address of the server (the default port is 6667) :\n" +
            "\n\n\n\n\n\n"
        )
    } else {
        if (!input.split(":")[1]) input += ":6667";
        db.servers.push(input);
        fs.writeFileSync("./db.json", JSON.stringify(db, null, 3))
        choseServeur();
    }
}

let channel = null;

function run(input) {
    input = input.split(/\s+/).join(" ");
    const args = input.split(/\s+/);
    process.stdout.write(`\n`);
    if (input.startsWith("//")) {
        client.write(`${input.slice(2)}\r\n`);
    } else if (input.startsWith("/")) {
        const command = args[0];
        switch (command) {
            case "/join":
                if (channel) client.write(`PART ${channel}\r\n`)
                client.write(`JOIN ${args[1]}\r\n`)
                channel = args[1];
                break;
            case "/mp":
                client.write(`PRIVMSG ${args[1]} :${args.slice(2).join(" ")}\r\n`);
                channel = args[1];
                break;
            case "/help":
                console.log("[clientIRC] This is the list of all the commands :\n" +
                    "[clientIRC] /help - get the list of all commands\n" +
                    "[clientIRC] /join <channel> - join a channel\n" +
                    "[clientIRC] /mp <user> - send a private message to a user\n" +
                    "[clientIRC] use //<command> for use any IRC command\n");
                break;

            default:
                console.log('Unknown command, type /help for the list of all commands')
                break;
        }
    } else {
        if (channel) {
            client.write(`PRIVMSG ${channel} :${input}\r\n`);
        } else {
            console.log("[clientIRC] please join a channel first")
        }
    }
}

function clear() {
    const blank = '\n'.repeat(process.stdout.rows)
    console.log(blank)
}

client.on('end', () => {
    if (state !== clientState.quit) {
        process.stdout.write(`\rConnection closed!`)

        setTimeout(() => {
            start();
        }, 2000);
    }
})

process.stdin.on('keypress', (c, k) => {
    showResults();
});

let lastLengthInput = 0;
function showResults() {
    let space = ""
    for (let i = 0; i < lastLengthInput; i++) {
        space += " ";
    }
    let string = rl.line
    let cursorDelta = 0;
    if (state == clientState.run) {
        if (rl.line.startsWith('/mp') && rl.line.split(/\s+/).length >= 3) {
            string = `[me -> ${rl.line.split(/\s+/)[1]}] ${rl.line.split(/\s+/).splice(2).join(' ')}`;
            cursorDelta = 4;
        } else {
            string = `${rl.line.startsWith("/") || rl.line.replace(/[ ,]+/, "").length === 0 ? "" : `<${name}> `}${rl.line}`;
        }
    } else if (state == clientState.password) {
        string = '*'.repeat(rl.line.length)
    }
    lastLengthInput = string.length;
    process.stdout.write(`\r${string}${space}`);
    readline.cursorTo(process.stdout, string.length - (string.length - rl.cursor) + cursorDelta);
}

client.on('data', (data) => {
    data = data.toString('utf-8');
    if (data.startsWith("PING")) {
        client.write(`PONG ${ip}\r\n`);
    } else {
        if (data.startsWith(':') && !data.startsWith(`:${ip}`)) {
            const args = data.split(" ");
            if (args[1] == "PRIVMSG" && args[2] == channel) {
                process.stdout.write(`\r<${args[0].slice(1).split('!')[0]}> ${data.split(":")[2].slice(0, -2)}\n`)
            } else if (args[2] == name) {
                process.stdout.write(`\r[${args[0].slice(1).split('!')[0]} -> me] ${data.split(":")[2].slice(0, -2)}\n`)
            } else if (args[1] == "JOIN") {
                process.stdout.write(`\r**${args[0].slice(1).split('!')[0]} a rejoin ${args[2].split(':')[1].slice(0, -2)}**\n`)
            } else if (args[1] == "PART") {
                process.stdout.write(`\r**${args[0].slice(1).split('!')[0]} a quité ${args[2].split(':')[1].slice(0, -2)}**\n`)
            } else {
                process.stdout.write("\rserver : " + data + "\n");
            }
        } else {
            process.stdout.write("\rserver : " + data + "\n");
        }
    }
});

rl.on('line', (line) => {

    switch (state) {
        case clientState.home:
            choseUser();
            break;
        case clientState.pseudo:
            choseUser(line);
            break;
        case clientState.server:
            choseServeur(line);
            break;
        case clientState.createServer:
            addServeur(line);
            break;
        case clientState.password:
            setPassword(false, line)
            break
        case clientState.run:
            run(line);
            break;
        default:
            break;
    }
});
rl.on('close', () => {
    state = clientState.quit
    if (client) {
        if (client.writable) client.write('QUIT\r\n');
        setTimeout(() => {
            client.end();
        }, 500);
    }
    clear()
    console.log('\t\tBye !\n\n\n\n')
});

// I am french sorry for my english ;-)



/*
    ====================================================================================
    This work is licensed under the Creative Commons
    Attribution-NonCommercial 4.0 International License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc/4.0/
    or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
    ====================================================================================
*/