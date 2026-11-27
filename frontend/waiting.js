const params = new URLSearchParams(location.search);
const code = params.get("code") || "";
const slot = params.get("slot") || "";
const $ = (id) => document.getElementById(id);

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
        const wr = await res.json();

        if (wr.status === "started") {
            // all players registered -> redirect to game
            location.href = "/game?code=" + encodeURIComponent(code) + "&slot=" + encodeURIComponent(slot);
            return;
        }
        if (wr.status === "expired") {
            showError("More then 10 minutes since the waiting room was established. Game dismissed.");
            return;
        }
        if (wr.status === "missing") {
            showError("This game does not exist (anymore).");
            return;
        }

        $("count").textContent = "Registered players: " + wr.registered + " / " + wr.humans;
        $("timer").textContent = "Remaining time: " + fmt(wr.secondsLeft) + " min";
    } 
    catch (e) {}
    
    setTimeout(poll, 1000);
}

poll();