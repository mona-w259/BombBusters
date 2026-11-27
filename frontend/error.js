const ERROR_TEXTS = {
    NoSuchGame:     "No game found with this code.",
    GameFull:       "This game is already full.",
    GameClosed:     "This game has already started or has expired.",
    TooFewPlayers:  "The amount of players is less then 4! They must sum up to exactly 4.",
    TooManyPlayers: "The amount of players is more then 4! They must sum up to exactly 4.",
    InvalidCode:    "The code must consist of exactly four digits!",
    Unknown:        "Something went wrong. Please try again."
};

const errorBox = document.getElementById("error");
const gameCodeInput = document.getElementById("game_code");

function showError(msg) {
    if (!errorBox) return;

    const icon = document.createElement("span");
    icon.className = "error-icon";
    icon.textContent = "!";

    const text = document.createElement("span");
    text.className = "error-text";
    text.textContent = msg;                 

    const close = document.createElement("button");
    close.type = "button";
    close.className = "error-close";
    close.setAttribute("aria-label", "Dismiss");
    close.textContent = "×";
    close.addEventListener("click", hideError);

    errorBox.replaceChildren(icon, text, close);
    errorBox.hidden = false;
}

if (gameCodeInput) {
    const form = gameCodeInput.form;
    form.noValidate = true;     
    
    form.addEventListener("submit", (e) => {
        if (e.submitter && e.submitter.formNoValidate) return;

        if (!gameCodeInput.checkValidity()) {
            e.preventDefault();
            showError("The game code must consist of exactly 4 digits.");
            gameCodeInput.focus();
            gameCodeInput.value = "";
        }
    });

    gameCodeInput.addEventListener("input", () => {
        gameCodeInput.classList.remove("input-invalid");
    });
}

function hideError() {
    if (errorBox) errorBox.hidden = true;
}


(function showUrlError() {
    const p = new URLSearchParams(location.search);
    const codeErr = p.get("error");
    if (!codeErr) return;

    showError(ERROR_TEXTS[codeErr] || ERROR_TEXTS.Unknown + "(" + codeErr + ")");

    p.delete("error");
    const clean = location.pathname + (p.toString() ? "?" + p : "");
    history.replaceState(null, "", clean);
})();
