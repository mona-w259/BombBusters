const params = new URLSearchParams(location.search);
const code = params.get("code") || "";
const slot = params.get("slot") || "";
let lastRacks= "";

async function poll() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/statusgame"
        const data = await (await fetch(url)).json();

        if (data.text == "defused" || data.text == "detonated") {
            loadGameStatus();
        } else {
            loadYRWires();
            loadDetonator();
            loadCurrTurn();
            loadActionMsg();
            loadResultMsg();
            pollRacks();
        }
        
    } catch (e) {
        console.error("pollState: ", e);
    }

    setTimeout(poll, 200);
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

function renderOwnRack(container, wires) {
    container.replaceChildren(...wires.map(w => {
        const cable = document.createElement("button");

        if (! w.isCut) {
            cable.className = "wire-" + w.color;
            cable.textContent = (w.hasHint) ? `💡${w.value}` : `${w.value}`;
            cable.disabled = false;
        } else {
            cable.className = "wire-" + w.color;
            cable.textContent = `${w.value}`;
            cable.disabled = true;
        }
        cable.addEventListener("click", () => sendInput(w.label));
        return cable;
    }));
}

function renderOtherRack(container, wires) {
    container.replaceChildren(...wires.map(w => {
        const cable = document.createElement("button");

        if (! w.isCut) {
            cable.className = "wire";
            cable.textContent = (w.hasHint) ? `💡${w.value}` : "";
            cable.disabled = false;
        } else {
            cable.className = "wire-" + w.color;
            cable.textContent = `${w.value}`;
            cable.disabled = true;
        }
        cable.addEventListener("click", () => sendInput(w.label));
        return cable;
    }));
}

async function pollRacks() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/racks?slot=" + encodeURIComponent(slot);
        const rack = await (await fetch(url, { cache: "no-store" })).json();

        if (!rack.error) {
            const rawRacks = JSON.stringify(rack);
            
            if (rawRacks !== lastRacks) {
                renderOwnRack(document.getElementById("rack-own"), rack.own.wires);
    
                rack.others.forEach((oth, i) => {
                    const el = document.getElementById("rack-other-" + (i + 1));
                    if (el) renderOtherRack(el, oth.wires);
                });
    
                lastRacks = rawRacks;
            } 
        }
    } catch (e) {
        console.error("pollRacks: ", e);
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

/*
    <div id="duo-cut">Duo Cut</div>
    <div id="solo-cut">Solo Cut</div>
    <div id="remove-red">Remove Red</div>
    <div id="choose-equipment">Choose Equipment</div>
*/
function renderActions(container, action) {
    container.replaceChildren(...actions.map(a => {
        const cable = document.createElement("button");
        cable.class = "action";// + (w.isUsed ? " used" : "");
        cable.disabled = a.isUsed;
        cable.textContent = `${w.value}`;
        cable.addEventListener("click", () => sendInput(w.label));
        return cable;
    }));
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
                statusG.textContent = "Oops...! Bomb detonated, game lost.";
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


/* get playerId's */
const idOwn = document.getElementById("player-id-own");
const idOth1 = document.getElementById("player-id-oth-1");
const idOth2 = document.getElementById("player-id-oth-2");
const idOth3 = document.getElementById("player-id-oth-3");

function getPlayerId() {
    idOwn.textContent = slot;
    
    const othIDs = [1, 2, 3, 4].filter(id => id !== Number(slot));  
    idOth1.textContent = othIDs[0];
    idOth2.textContent = othIDs[1];
    idOth3.textContent = othIDs[2];
}


/* function calls */
getPlayerId();
loadYRWires();
poll();