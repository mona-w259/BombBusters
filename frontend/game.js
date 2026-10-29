const params = new URLSearchParams(location.search);
const code = params.get("code") || "";
const slot = params.get("slot") || "";
let lastRacks = "";

async function poll() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/statusgame"
        const data = await (await fetch(url)).json();

        if (data.text == "defused" || data.text == "detonated") {
            loadGameStatus();
        } else {
            loadDetonator();
            loadCurrTurn();
            loadActionMsg();
            loadResultMsg();
            pollRacks();
            loadChoosePlayerIds();
            loadAwaiting();
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
        const value = getValueWire(w);

        if (! w.isCut) {
            cable.className = "wire-" + w.color;
            cable.textContent = (w.hasHint) ? `💡${value}` : `${value}`;
            cable.disabled = false;
        } else {
            cable.className = "wire-" + w.color;
            cable.textContent = `${value}`;
            cable.disabled = true;
        }
        cable.addEventListener("click", () => sendInput(w.label));
        return cable;
    }));
}

function renderOtherRack(container, wires) {
    container.replaceChildren(...wires.map(w => {
        const cable = document.createElement("button");
        const value = getValueWire(w);

        if (! w.isCut) {
            cable.className = "wire";
            cable.textContent = (w.hasHint) ? `💡${value}` : " ";
            cable.disabled = false;
        } else {
            cable.className = "wire-" + w.color;
            cable.textContent = `${value}`;
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

function getValueWire(w) {
    let valAddOn = "";

    switch (w.color) {
        case "yellow":
            valAddOn = ".1";
            break;
        case "red":
            valAddOn = ".5";
            break;
    }
    
    return w.value + valAddOn;
}


/* announce current turn */
const annTurn = document.getElementById("currturn");

async function loadCurrTurn() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/turn"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            annTurn.textContent = "Player " + data.text + " - it's your turn!";
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
        statusG.textContent = "(error): " + e;
    }
}


document.addEventListener("click", (e) => {
    const btn = e.target.closest("[data-input]");
    if (btn) sendInput(btn.dataset.input);
});

async function sendInput(text) {
    try {
        const res = await fetch("/api/game/" + encodeURIComponent(code) + "/input", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ slot: Number(slot), text })
        });
        const d = await res.json();
        if (!d.ok) console.warn("Input rejected: ", d.error);
    } catch (e) {
        console.warn("Server not reachable");
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


const idBtnOth1 = document.getElementById("id-btn-1");
const idBtnOth2 = document.getElementById("id-btn-2");
const idBtnOth3 = document.getElementById("id-btn-3");

async function loadChoosePlayerIds() {
    try {
        idBtnOth1.textContent = "Player " + idOth1.textContent;
        idBtnOth1.dataset.input = Number(idOth1.textContent);

        idBtnOth2.textContent = "Player " + idOth2.textContent;
        idBtnOth2.dataset.input = Number(idOth2.textContent);        
        
        idBtnOth3.textContent = "Player " + idOth3.textContent;
        idBtnOth3.dataset.input = Number(idOth3.textContent);
    } 
    catch (e) {
        idBtnOth1.textContent = "(error): " + e;
        idBtnOth2.textContent = "(error): " + e;
        idBtnOth3.textContent = "(error): " + e;
    }
}


/* load state of shown action */
const sections = {
    menu:      document.querySelector(".action-container"),
    equipment: document.querySelector(".equipment-container"),
    player:    document.querySelector(".choose-player-id-container"),
};

async function loadAwaiting() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/awaiting?slot=" + encodeURIComponent(slot);
        const data = await (await fetch(url, { cache: "no-store" })).json();
        if (data.text === undefined) return;

        Object.entries(sections).forEach(([kind, el]) => { el.hidden = (kind !== data.text); });
        document.body.dataset.awaiting = data.text;
    } catch (e) {
        console.error("loadAwaiting: ", e);
    }
}


/* function calls */
getPlayerId();
loadYRWires();
poll();