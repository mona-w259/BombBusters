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

loadYRWires();


poll();