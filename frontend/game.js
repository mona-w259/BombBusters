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
        yrWires.textContent = "(error)";
    }
    
    setTimeout(loadYRWires, 1000);
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
            return;
        }
    } catch (e) {
        detonator.textContent = "(error)";
    }

    setTimeout(loadDetonator, 1000);
}


/* print racks */
const rack1 = document.getElementById("rack1");
const rack2 = document.getElementById("rack2");
const rack3 = document.getElementById("rack3");
const rack4 = document.getElementById("rack4");

async function loadRacks() {
    try {
        const url1 = "/api/game" + encodeURIComponent(code) + "/racklabled?slot=" + encodeURIComponent(slot)
        const dataLab = await (await fetch(url1)).json();

        const url2 = "/api/game" + encodeURIComponent(code) + encodeURIComponent(slot) + "/rackoverview"
        const dataOvv = await (await fetch(url2)).json();

        if (dataLab.text != undefined && dataOvv.text != undefined) {
            switch (slot) {
                // data.racks.forEach(([pid, text]) => ...)
                case 1:
                    rack1.textContent = dataLab.text;
                    rack2.textContent = dataOvv.text;
                    rack3.textContent = dataOvv.text;
                    rack4.textContent = dataOvv.text;
                case 2:
                    rack1.textContent = dataOvv.text;
                    rack2.textContent = dataLab.text;
                    rack3.textContent = dataOvv.text;
                    rack4.textContent = dataOvv.text;
                case 3:
                    rack1.textContent = dataOvv.text;
                    rack2.textContent = dataOvv.text;
                    rack3.textContent = dataLab.text;
                    rack4.textContent = dataOvv.text;
                case 4:
                    rack1.textContent = dataOvv.text;
                    rack2.textContent = dataOvv.text;
                    rack3.textContent = dataOvv.text;
                    rack4.textContent = dataLab.text;
                default:
                    rack1.textContent = "Unknown slot";
                    rack2.textContent = "Unknown slot";
                    rack3.textContent = "Unknown slot";
                    rack4.textContent = "Unknown slot";
            } 
            return;
        }
    } catch (e) {
        rack1.textContent = "(error)";
        rack2.textContent = "(error)";
        rack3.textContent = "(error)";
        rack4.textContent = "(error)";
    }

    setTimeout(loadRacks, 1000);
}


/* announce turn */
const annTurn = document.getElementById("currturn");

async function loadCurrTurn() {
    try {
        const url = "/api/game" + encodeURIComponent(code) + "/turn"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            annTurn.textContent = "Current turn: " + data.text;
            return;
        }
    } catch (e) {
        annTurn.textContent = "(error)";
    }

    setTimeout(loadCurrTurn, 1000);
}

/* print current action */
const actionMsg = document.getElementById("action");

async function loadActionMsg() {
    try {
        const url = "/api/game" + encodeURIComponent(code) + "/actionmessage"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            actionMsg.textContent = "Action: " + data.text;
            return;
        }
    } catch (e) {
        actionMsg.textContent = "Currently no actions.";
    }

    setTimeout(loadActionMsg, 1000);
}


/* print result of last action */
const resultMsg = document.getElementById("result");

async function loadResultMsg() {
    try {
        const url = "/api/game" + encodeURIComponent(code) + "/resultmessage"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            resultMsg.textContent = "Result: " + data.text;
            return;
        }
    } catch (e) {
        resultMsg.textContent = "";
    }

    setTimeout(loadResultMsg, 1000);
}


loadYRWires();
loadDetonator();
loadActionMsg();
loadResultMsg();

poll();