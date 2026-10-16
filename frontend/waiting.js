const params = new URLSearchParams(location.search);
const code = params.get("code") || "";
const slot = params.get("slot") || "";
const $ = (id) => document.getElementById(id);

// textContent statt innerHTML: der Code kommt aus der URL und ist nicht vertrauenswürdig
$("code").textContent = code;

function fmt(sec) {
    const m = Math.floor(sec / 60);
    const s = String(sec % 60).padStart(2, "0");
    return m + ":" + s;
}

function stop(msg) {
    $("message").textContent = msg;
    $("count").textContent = "";
    $("timer").textContent = "";
}

async function poll() {
    try {
        const res = await fetch("/api/waitingRoom/" + encodeURIComponent(code));
        const s = await res.json();

        if (s.status === "started") {
            // all players registered -> redirect to game
            location.href = "/game?code=" + encodeURIComponent(code) + "&slot=" + encodeURIComponent(slot);
            return;
        }
        if (s.status === "expired") {
            stop("More then 10 minutes since the waiting room was established. Dismiss game.");
            return;
        }
        if (s.status === "missing") {
            stop("This game does not exist (anymore).");
            return;
        }

        $("count").textContent = "Registered players: " + s.registered + " / " + s.humans;
        $("timer").textContent = "Remaining time: " + fmt(s.secondsLeft) + " min";
    } catch (e) {
        // Server kurz nicht erreichbar: einfach weiter versuchen
    }
    setTimeout(poll, 1000);
}

poll();