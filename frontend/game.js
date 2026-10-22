const params = new URLSearchParams(location.search);
const code = params.get("code") || "";
const slot = params.get("slot") || "";

async function poll() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/statusgame"
        const data = await (await fetch(url)).json();

        if (data.text == "defused" || data.text == "detonated") {
            loadGameStatus();
        } else {
            loadYRWires();
            loadDetonator();
            loadRacks();
            loadCurrTurn();
            loadActionMsg();
            loadResultMsg();
        }
        
    } catch (e) {

    }

    setTimeout(poll, 100);
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
}


/* announce current turn */
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
}

/* print end state */
const statusG = document.getElementById("gamestatusend");
const overlay = document.getElementById("overlay");

async function loadGameStatus() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/statusgame"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            if (data.text == "detonated") {
                overlay.style.display = "flex";
                statusG.textContent = "<h1>Congratulation!<\h1>Bomb detonated, game lost.";
            } else if (data.text == "defused") {
                overlay.style.display = "flex";
                statusG.textContent = "Bomb defused, game won.";
            } else {
                statusG.textContent = "Ongoing game.";
            }
            return;
        }
    } catch (e) {
        actionMsg.textContent = "(error): " + e;
    }
}


poll()