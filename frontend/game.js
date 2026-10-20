const params = new URLSearchParams(location.search);
const code = params.get("code") || "";
const slot = params.get("slot") || "";
const log = document.getElementById("log");
let since = 0;   // Index der nächsten noch nicht gesehenen Nachricht


async function poll() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/output"
                  + "?slot=" + encodeURIComponent(slot) + "&since=" + since;
        const data = await (await fetch(url)).json();

        if (data.messages) {
            if (data.messages.length > 0) {
                // nur automatisch scrollen, wenn der Spieler schon unten war
                const atBottom = log.scrollTop + log.clientHeight >= log.scrollHeight - 20;
                // textContent statt innerHTML: Spieltexte nie als HTML interpretieren
                log.textContent += data.messages.join("");
                if (atBottom) log.scrollTop = log.scrollHeight;
            }
            since = data.next;
        }
    } catch (e) {
    // Server kurz nicht erreichbar: einfach weiter versuchen
    }
    setTimeout(poll, 500);
}


/* printYRWires */
const yrWires = document.getElementById("yr-wires");

async function loadYRWires() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/yrwires"
        const data = await (await fetch(url)).json();

        if (data.text !== undefined) {
            yrWires.textContent = data.text;
            return;
        }
    } catch (e)  {
        yrWires.textContent = "(error): " + e;
    }
}


/* showDetonator */
const detonator = document.getElementById("lifeValue");
const lifePointers = document.querySelectorAll("#lifeGauge [data-lpoint]");

async function loadDetonator() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/detonator"
        const data = await (await fetch(url)).json();

        if (data.text !== undefined) {
            detonator.textContent = data.text;

            /* show only the point associated to the detonator value */
            const detValue = Number(data.text);
            if (Number.isNaN(detValue)) return;

            lifePointers.forEach (point => {
                point.style.display = (Number(point.dataset.lpoint) === data.text) ? "" : "none";
            })
        }
    } catch (e) {
        detonator.textContent = "(error): " + e;
    }

    setTimeout(loadDetonator, 100);
}


/* print racks */
const rack1 = document.getElementById("rack-1");
const rack2 = document.getElementById("rack-2");
const rack3 = document.getElementById("rack-3");
const rack4 = document.getElementById("rack-4");

async function loadRacks() {
    try {
        const url1 = "/api/game/" + encodeURIComponent(code) + "/racklabled?slot=" + encodeURIComponent(slot)
        const dataLab = await (await fetch(url1)).json();

        const url2 = "/api/game/" + encodeURIComponent(code) + "/rackoverview?slot="
        const dataOvv1 = await (await fetch(url2 + "1")).json();
        const dataOvv2 = await (await fetch(url2 + "2")).json();
        const dataOvv3 = await (await fetch(url2 + "3")).json();
        const dataOvv4 = await (await fetch(url2 + "4")).json();

        rack1.textContent = dataOvv1.text;
        rack2.textContent = dataOvv2.text;
        rack3.textContent = dataOvv3.text;
        rack4.textContent = dataOvv4.text;
/*
        if (dataLab.text != undefined) {
            switch (slot) {
                case 1:
                    rack1.textContent = dataLab.text;
                case 2:
                    rack2.textContent = dataLab.text;
                case 3:
                    rack3.textContent = dataLab.text;
                case 4:
                    rack4.textContent = dataLab.text;
                default:
                    rack1.textContent = "Unknown slot";
                    rack2.textContent = "Unknown slot";
                    rack3.textContent = "Unknown slot";
                    rack4.textContent = "Unknown slot";
            } 
            return;
        }
            */
    } catch (e) {
        rack1.textContent = "(error): " + e;
        rack2.textContent = "(error): " + e;
        rack3.textContent = "(error): " + e;
        rack4.textContent = "(error): " + e;
    }

    setTimeout(loadRacks, 100);
}


/* announce turn */
const annTurn = document.getElementById("currturn");

async function loadCurrTurn() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/turn"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            annTurn.textContent = "Current turn: Player " + data.text + " - it's your turn!";
        }
    } catch (e) {
        annTurn.textContent = "(error): " + e;
    }

    setTimeout(loadCurrTurn, 100);
}

/* print current action */
const actionMsg = document.getElementById("action");

async function loadActionMsg() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/actionmessage"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            actionMsg.textContent = "Action: " + data.text;
        }
    } catch (e) {
        actionMsg.textContent = "(error): " + e;
    }

    setTimeout(loadActionMsg, 100);
}


/* print result of last action */
const resultMsg = document.getElementById("result");

async function loadResultMsg() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/resultmessage"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            resultMsg.textContent = "Result: " + data.text;
        }
    } catch (e) {
        resultMsg.textContent = "(error): " + e;
    }

    setTimeout(loadResultMsg, 100);
}

/* print end state */
const endState = document.getElementById("end");
const overlay = document.getElementById("overlay");

async function loadEndState() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/endstate"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            if (data.text == 0) {
                endState.textContent = "<h1>Congratulation!<\h1>Bomb detonated, game lost.";
                overlay.style.display = "flex";
            } else if (data.text == 1) {
                endState.textContent = "Bomb defused, game won.";
                overlay.style.display = "flex";
            } else {
                endState.textContent = "Ongoing game.";
            }
            return;
        }
    } catch (e) {
        actionMsg.textContent = "(error): " + e;
    }

    setTimeout(loadEndState, 100);
}



loadYRWires();
loadDetonator();
loadRacks();
loadCurrTurn();
loadActionMsg();
loadResultMsg();
loadEndState();

poll();