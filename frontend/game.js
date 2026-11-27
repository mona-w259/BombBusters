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
            loadRackToChooseFrom();
            pollRacks();
            loadWireToChooseFrom();
            loadChoosePlayerIds();
            loadAwaiting();
            pollActions();
            pollEquipment();
        }
        
    } catch (e) {
        console.error("pollState: ", e);
    }

    setTimeout(poll, 200);
}


/* print yellow and red wires */
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


/* show detonator */
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
                point.style.display = (Number(point.dataset.lpoint) === detValue) ? "" : "none";
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

            const rawVal = String(w.value).replace("💡", "");
            cable.classList.toggle(rawVal > 9 ? "hint-two-digits" : "hint", w.hasHint);
            cable.disabled = false;
        } else {
            cable.className = "wire-" + w.color;
            cable.textContent = `${value}`;
            cable.disabled = true;
            cable.classList.toggle("cutted", true);
        }
        cable.addEventListener("click", () => sendInput(w.label));
        return cable;
    }));
}

function renderOtherRack(container, wires, id) {
    container.replaceChildren(...wires.map(w => {
        const cable = document.createElement("button");
        const value = getValueWire(w);

        if (! w.isCut) {
            cable.className = "wire";
            cable.textContent = (w.hasHint) ? `💡${value}` : " ";
            cable.classList.toggle(w.value == "🟡" ? "hint-yellow" : (w.value > 9 ? "hint-two-digits" : "hint"), w.hasHint);
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
                    if (el) renderOtherRack(el, oth.wires, i+1);
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
    const raw = String(w.value).replace("💡", "");

    switch (w.color) {
        case "yellow":
            valAddOn = ".1";
            break;
        case "red":
            valAddOn = ".5";
            break;
    }
    
    return raw + valAddOn;
}


/* print action black/white, when unusable */

function renderAction(a) {
    const btn = document.querySelector(`.action-btn[data-input="${a.key}"]`);
    if (!btn) return;

    const item = btn.closest(".action-item");
    const unusable = !a.usable;

    btn.disabled = unusable;
    item.classList.toggle("unusable", unusable);
}

async function pollActions() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/actions?slot=" + encodeURIComponent(slot);
        const a = await (await fetch(url, { cache: "no-store" })).json();

        a.actions.forEach(act => {
            renderAction(act);
        });
    } 
    catch (e) {
        console.error("pollActions: ", e);
    }
}


/* print equipment black/white, when used */

function renderEquipment(eq) {
    const btn = document.querySelector(`.equipment-btn[data-input="${eq.name}"]`);
    if (!btn) return;

    const item = btn.closest(".equipment-item");
    const locked = !eq.unlocked;

    item.classList.toggle("used", eq.used);
    item.classList.toggle("locked", locked);
    btn.disabled = locked || eq.used;
    if (btn.disabled) selectedEquipment.delete(eq.name);
}

async function pollEquipment() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/equipment?slot=" + encodeURIComponent(slot);
        const eq = await (await fetch(url, { cache: "no-store" })).json();

        eq.otherEquipment.forEach(e => {
            renderEquipment(e);
        });
        renderEquipment(eq.personalEquipment);
        renderEquipmentSelection();
    } 
    catch (e) {
        console.error("pollEquipment: ", e);
    }
}


/* announce current turn */
const annTurn = document.getElementById("currturn");

async function loadCurrTurn() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/turn"
        const data = await (await fetch(url)).json();

        if (data.text != undefined) {
            annTurn.textContent = "Player " + data.text + (Number(data.text) === Number(slot) ? " - it's your turn!" : "");
        }
    } catch (e) {
        annTurn.textContent = "(error): " + e;
    }
}


/* print current action */
const actionMsg = document.getElementById("action");

async function loadActionMsg() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/actionmessage?slot=" + encodeURIComponent(slot);
        const data = await (await fetch(url, { cache: "no-store" })).json();

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
        const url = "/api/game/" + encodeURIComponent(code) + "/resultmessage?slot=" + encodeURIComponent(slot);
        const data = await (await fetch(url, { cache: "no-store" })).json();

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


/* get playerId's */
const idOwn = document.getElementById("player-id-own");
const idOth1 = document.getElementById("player-id-oth-1");
const idOth2 = document.getElementById("player-id-oth-2");
const idOth3 = document.getElementById("player-id-oth-3");
let othIDs = [];  

function getPlayerId() {
    idOwn.textContent = slot;
    
    othIDs = [1, 2, 3, 4].filter(id => id !== Number(slot));  
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


/* highlight rack where wire should be chosen from */

let rackToChooseFrom = 0;
let playerWhoChoosesR = 0;

async function loadRackToChooseFrom() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/choosingrack";
        const data = await (await fetch(url)).json();

        if (data != undefined) {
            rackToChooseFrom = Number(data.racktochoose);
            playerWhoChoosesR = Number(data.playerwhochoosesrack);
        }

        if (rackToChooseFrom == 0) {
            document.querySelectorAll(".rack-inner.highlighted")
                .forEach(el => el.classList.remove("highlighted"));
        }

        if (Number(slot) == playerWhoChoosesR) {
            if (rackToChooseFrom >= 1 && rackToChooseFrom <= 4) {

                const isYourself = !othIDs.includes(rackToChooseFrom);

                if (isYourself) {
                    highlightRack("rack-own");
                } 
                else {
                    const id = othIDs.indexOf(rackToChooseFrom);
                    highlightRack("rack-other-" + (id + 1));
                }
            }
        }
    }
    catch (e) {
        console.error("(error): ", e);
    }
}

function highlightRack(idName) {
    const rack = document.getElementById(idName);
    if (!rack) return console.warn("rack id not found:", idName);

    rack.classList.add("highlighted");
}


/* highlight wires where one should be chosen of */

let wiresToChooseFrom = [];
let playerWhoChoosesW = 0;

async function loadWireToChooseFrom() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/choosingwire";
        const data = await (await fetch(url)).json();

        if (data != undefined) {
            wiresToChooseFrom = data.wirestochoose;
            playerWhoChoosesW = Number(data.playerwhochooseswire);
        }

        if (wiresToChooseFrom.length == 0) {
            document.querySelectorAll(".wire-blue.highlighted")
                .forEach(el => el.classList.remove("highlighted"));

            document.querySelectorAll(".wire-yellow.highlighted")
                .forEach(el => el.classList.remove("highlighted"));

            document.querySelectorAll(".wire-red.highlighted")
                .forEach(el => el.classList.remove("highlighted"));
        }

        if (Number(slot) == playerWhoChoosesW) {
            highlightWires(wiresToChooseFrom);
        }
    }
    catch (e) {
        console.error("(error): ", e);
    }
}

function highlightWires(wires) {
    const rack = document.getElementById("rack-own");

    wires.forEach(i => {
        const el = rack.children[i];
        if (el) el.classList.add("highlighted");
    });
}


/* load state of shown action */
const sections = {
    menu:      document.querySelector(".action-container"),
    player:    document.querySelector(".choose-player-id-container"),
    equipment: document.querySelector(".equipment-container"),
};

async function loadAwaiting() {
    try {
        const url = "/api/game/" + encodeURIComponent(code) + "/awaiting?slot=" + encodeURIComponent(slot);
        const data = await (await fetch(url, { cache: "no-store" })).json();
        if (data.text === undefined) return;

        Object.entries(sections).forEach(([kind, el]) => { el.hidden = (kind !== data.text); });
        document.body.dataset.awaiting = data.text;
        
        if (data.text !== "equipment" && selectedEquipment.size > 0) {
            selectedEquipment.clear();
            renderEquipmentSelection();
        }
    } 
    catch (e) {
        console.error("loadAwaiting: ", e);
    }
}

/* show selected equipment with a check */
const selectedEquipment = new Set();

function renderEquipmentSelection() {
    document.querySelectorAll(".equipment-btn").forEach(btn => {
        const sel = selectedEquipment.has(btn.dataset.input);
        btn.classList.toggle("selected", sel);
    })
}

function toggleEquipment(name) {
    if (selectedEquipment.has(name)) {
        selectedEquipment.delete(name);
    }
    else {
        selectedEquipment.add(name);
    }
    renderEquipmentSelection();
}


/* submit equipment */
let submittingEquipment = false;

async function submitEquipment() {
    if (submittingEquipment) return;

    const names = [...selectedEquipment];

    submittingEquipment = true;
    try {
        for (const name of names) {
            await sendInput(name);
        }
        if (names.contains("Stabilizer")) await sendInput("0");
        
        selectedEquipment.clear();
        renderEquipmentSelection();
    }
    finally {
        submittingEquipment = false;
    }
}


/* input functions */

document.addEventListener("click", (e) => {
    const btn = e.target.closest("[data-input]");
    if (!btn) return;

    if (btn.classList.contains("equipment-btn")) {
        toggleEquipment(btn.dataset.input);
    } 
    else if (btn.id == "equipment-submit") {
        submitEquipment();
    } 
    else if (btn.dataset.input === "back") {
        selectedEquipment.clear();
        renderEquipmentSelection();
        sendInput(btn.dataset.input);
    }
    else {
        sendInput(btn.dataset.input);
    }
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
        return { ok: false, error: "Server not reachable" };
    }
}


/* function calls */

getPlayerId();
loadYRWires();
poll();