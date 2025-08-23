let settings_window;
let searchbar;
let search_results;
let main_content;
let last_search = "";

function change_colorscheme(e) {
    localStorage.setItem("colorscheme", e.target.value);
    document.documentElement.setAttribute("color-scheme", e.target.value);
}

function open_close_settings() {
    if (settings_window.classList.contains("hidden")) settings_window.classList.remove("hidden");
    else settings_window.classList.add("hidden");
}

function run_search() {
    const input = searchbar.value;
    if (input == last_search) return;
    console.log("Running search with", input);
}

function search_change(e) {
    if (e.key == "Escape") {
        searchbar.blur();
    } else if (e.key == "Enter") {
        clearTimeout(search_timeout);
        run_search();
    } else {
        clearTimeout(search_timeout);
        search_timeout = setTimeout(run_search, 1000);
    }
}

function focus_search() {
    search_results.classList.remove("hidden");
    main_content.classList.add("hidden");
}

function blur_search() {
    search_results.classList.add("hidden");
    main_content.classList.remove("hidden");
}

function onload() {
    let colorscheme = localStorage.getItem("colorscheme");
    settings_window = document.getElementsByClassName("settings-popup")[0];
    document.getElementsByClassName("settings-button")[0].addEventListener("click", open_close_settings);
    if (!["ayu", "dark", "light", "preference"].includes(colorscheme)) {
        colorscheme = "preference";
        localStorage.setItem("colorscheme", colorscheme);
    }
    document.documentElement.setAttribute("color-scheme", colorscheme);

    let radio_buttons = document.querySelectorAll("input[type='radio'][name='theme']");
    for (let i = 0; i < radio_buttons.length; ++i) {
        radio_buttons[i].checked = radio_buttons[i].value == colorscheme;
        radio_buttons[i].addEventListener("click", change_colorscheme);
    }

    searchbar = document.getElementsByClassName("search-input")[0];
    searchbar.addEventListener("keydown", search_change);
    searchbar.addEventListener("focus", focus_search);
    searchbar.addEventListener("blur", blur_search);

    search_results = document.getElementsByClassName("search-results")[0];
    main_content = document.getElementsByClassName("main-content")[0];
}

let search_timeout = 0;

window.addEventListener("load", onload);
