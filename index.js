let settings_window;
let searchbar;
let search_results;
let settings_button;
let expand_button;
let main_content;
let last_search = "";
let running_search = false;

window.search_index = undefined;
window.init_search = undefined;

async function load_search_index() {
    if (window.search_index != undefined) return;
    try {
        let element = document.createElement("script");
        element.src = `${root}searchidx.js`;
        await new Promise((resolve, err) => {
            element.addEventListener("error", (e) => { console.log(e); err(e); });
            window.init_search = resolve;
            document.head.append(element);
        });
    } catch (e) {
        console.error("Failed to load search index:", e);
        window.search_index = "error";
    }
}

function change_colorscheme(e) {
    localStorage.setItem("colorscheme", e.target.value);
    document.documentElement.setAttribute("color-scheme", e.target.value);
}

function open_close_settings() {
    if (settings_window.classList.contains("hidden")) settings_window.classList.remove("hidden");
    else settings_window.classList.add("hidden");
}

function filter(input) {
    if (input.length == 0) return [];
    return window.search_index.filter(v => v[1][v[1].length - 1].includes(input)).sort((a, b) => a.length - b.length);
}

async function run_search() {
    const input = searchbar.value;
    if (input == last_search || running_search) return;
    running_search = true;
    await load_search_index();
    last_search = input;
    const results = filter(input);
    while(search_results.children.length > 0) search_results.removeChild(search_results.children[0]);
    if (results.length == 0) {
        search_results.classList.add("hidden");
        main_content.classList.remove("hidden");
        running_search = false;
        return;
    }
    else focus_search();
    let i = 0;
    for (const result of results) {
        let elem = document.createElement("a");
        elem.href = `${root}${result[0]}`;
        elem.setAttribute("data-index", i++);
        elem.classList.add("searchpath");
        for (let i = 0; i < result[1].length; ++i) {
            if(i != 0) {
                let sep = document.createElement("span");
                sep.textContent = "::";
                elem.append(sep)
            }
            elem.append(result[1][i]);
        }
        search_results.append(elem);
    }
    running_search = false;
}

function search_change(e) {
    if (e.key == "Escape") {
        searchbar.blur();

        // force blur
        search_results.classList.add("hidden");
        main_content.classList.remove("hidden");
    } else if (e.key == "Enter") {
        clearTimeout(search_timeout);
        run_search();
    } else if (e.key == "ArrowDown" && search_results.children.length != 0 && last_search.length != 0) {
        searchbar.blur();
        search_results.children[0].focus();
    } else {
        clearTimeout(search_timeout);
        search_timeout = setTimeout(run_search, 500);
    }
}

function focus_search() {
    searchbar.placeholder = "type to search here";
    if (last_search.length == 0) return;
    search_results.classList.remove("hidden");
    main_content.classList.add("hidden");
}

function blur_search() {
    searchbar.placeholder = "press s or / to start searching";
    if (last_search.length != 0) return;
    search_results.classList.add("hidden");
    main_content.classList.remove("hidden");
}

function keypress(e) {
    if (e.target.tagName == "INPUT") return;
    if (search_results.classList.contains("hidden")) {
        if (e.key == "/" || e.key == "s") {
            e.preventDefault();
            searchbar.focus();
        }
    } else {
        if (e.key == "Escape") {
            search_results.classList.add("hidden");
            main_content.classList.remove("hidden");
            searchbar.blur();
        } else if (e.key == "ArrowDown" && document.activeElement) {
            let num = Number(document.activeElement.getAttribute("data-index")) + 1;
            if (search_results.children.length > num) search_results.children[num].focus();
        } else if (e.key == "ArrowUp" && document.activeElement) {
            let num = Number(document.activeElement.getAttribute("data-index")) - 1;
            if (num >= 0) search_results.children[num].focus();
            else searchbar.focus();
        } else if (e.key == "ArrowDown" || e.key == "ArrowUp") {
            searchbar.focus();
        }
    }
}

function handle_settings_blur_click(e) {
    if (!e.target || settings_window.classList.contains("hidden")) return;
    let target = e.target;
    if (e.target == settings_window || e.target == settings_button) return;
    while (target) {
        target = target.parentElement;
        if (target == settings_window || target == settings_button) return;
    }
    e.preventDefault();
    settings_window.classList.add("hidden")
}

function expand_click() {
    expand_button.classList.toggle("open");
    let expanded = expand_button.classList.contains("open");
    expand_button.textContent = expanded ? "Summary" : "Show all";
    const elems = document.getElementsByTagName("details");
    for (let i = 0; i < elems.length; ++i) elems[i].open = expanded;
}

function onload() {
    let colorscheme = localStorage.getItem("colorscheme");
    settings_window = document.getElementsByClassName("settings-popup")[0];
    settings_button = document.getElementsByClassName("settings-button")[0];
    expand_button = document.getElementsByClassName("expand-button")[0];
    settings_button.addEventListener("click", open_close_settings);
    expand_button.addEventListener("click", expand_click);
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
    window.addEventListener("keydown", keypress);
    window.addEventListener("click", handle_settings_blur_click);

    search_results = document.getElementsByClassName("search-results")[0];
    main_content = document.getElementsByClassName("main-content")[0];
}

let search_timeout = 0;

window.addEventListener("load", onload);
