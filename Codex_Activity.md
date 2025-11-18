# Macalester DS 456 - Projects in Data Science - Codex Activity

*Shilad Sen (with help from Codex), 2025/11/25*

Lilabeth & Wesley

In this activity you will:

- Use Codex as a collaborator on a real spatial dataset.
- Practice being *critical* of Codex’s suggestions (not just accepting them).
- Compare different prompting styles (one-shot vs. planned) for research questions.

This activity was developed on a Mac in November 2025. It should work on other platforms, but tools change quickly—if something breaks, document it and ask questions!

# 0. Get the project

- Fork the [Codex Activity](https://github.com/shilad/ds-456-codex-activity) on GitHub.
- Clone **your fork** (not the original) to your computer.

# 1. Choose your tools and set up Codex

You may work in **Python (VS Code)** or **R (VS Code or RStudio)**.

## 1.1 Highly Recommended: VS Code + Codex (Python or R)

- Install [VS Code](https://code.visualstudio.com/).
- Install the [Codex VS Code extension](https://developers.openai.com/codex/ide/).
- If you are using R, also install the [VS Code R extension](https://marketplace.visualstudio.com/items?itemName=reditorsupport.r).
- Open your cloned repo folder in VS Code.
- In the Codex sidebar:
  - Click “Use API Key” and use the key Shilad gave you.  
    **DO NOT PUT THIS KEY ANYWHERE IN YOUR GITHUB REPO.**
  - Open Codex settings → “Open `config.toml`” and set it to:
    ```toml
    model = "gpt-5.1"
    model_reasoning_effort = "medium"

    [tools]
    web_search_request = true

    [sandbox_workspace_write]
    network_access = true
    ```
  - Restart VS Code after editing `config.toml`.

## 1.2 Optional: RStudio + Codex in a separate terminal

If you strongly prefer RStudio:

- Use **RStudio** for your R scripts / Rmd files.
- Using a terminal, install the Codex CLI: `npm i -g @openai/codex`
- In a terminal, run `codex` once so it creates a config file at `~/.codex/config.toml`.
- Open `~/.codex/config.toml` in any text editor you like (VS Code, TextEdit, Notepad, `nano`, etc.) and paste the same settings shown in 1.1.
- In a separate terminal window (**from the repo root**), run Codex and use it to:
  - Draft and refactor R code.
  - Ask questions about errors.
- You are still responsible for keeping all code and writeups in this repo so they can be graded.

# 2. Create your assignment skeleton

You will **not** edit this `README.md` directly. Instead:

- Copy `README.md` to `Codex_Activity.md`.
- Add your name(s) at the top of `Codex_Activity.md`.
- For every prompt marked **Task:** below, you will write your answers in `Codex_Activity.md`.
- Your actual analysis code should live in code files, for example:
  - R: `analysis/codex_activity.Rmd` or `analysis/codex_activity.R`
  - Python: `analysis/codex_activity.ipynb` or `analysis/codex_activity.py`

You may reference your code files from `Codex_Activity.md` (e.g., “see Figure 1 in `analysis/codex_activity.Rmd`”).


# 3. Early dataset interactions

- Pick a spatial dataset from [Open Data Minneapolis](https://opendata.minneapolismn.gov).  
  One fun option (used in the instructor example) is  
  [Motorized Foot & eBike Trips in 2023](https://opendata.minneapolismn.gov/datasets/95547087876344e592dbdfa6244bbc43_0/explore).
- Give Codex the URL for the dataset and ask it to help you get started understanding the data.  
  Keep your request intentionally vague (e.g., “Help me explore this dataset”).

**Task 3.1 (in `Codex_Activity.md`):**  
What do you notice about this early interaction? Consider:
- What did Codex assume or gloss over?
- Did it make any mistakes or questionable choices?
- How clear or explainable was the code it produced?

ANSWER: It hallucinated a variable, "customer", which does not exist in the real dataset. It gives us a plan for us to implement as users, not something it will run itself. It glossed over the fact that this dataset only involves tows from one snow emergency, over the span of a few days. The code it produced seems pretty clear and simple. 

# 4. More directed exploration

- If you are partner coding, *switch partners*.
- Download the dataset you would like to analyze and move it under `data/`.
- Ask Codex to help you analyze the **basic information** about the dataset:
  - basic summaries (rows, columns, variable types),
  - simple visualizations,
  - any surprising patterns.
- Then ask Codex for specific improvements (e.g., “Use `dplyr` instead of base R”, “Add labels and titles”, “Handle missing values more carefully”).

**Task 4.1 (in `Codex_Activity.md`):**
- What did Codex do well?
- What would you have done differently if you wrote the code yourself?
- Looking at the code Codex wrote, what could be improved (style, clarity, correctness, efficiency)?

It gave us a good summary, but the code it suggested didn't work, and it struggled (but ultimately succeeded) to debug it. It also didn't create the hour column correctly, and thought all the tows were at 7 pm. Other than that, the code looks clear and efficient, and it's probably similar to how we would've written it.

# 5. Mapping the data

- If you are partner coding, *switch partners*.
- Ask Codex to help you map your data. Pay attention to what happens at first:
  - Does it correctly identify what is needed to make a map?
  - Does it confuse attributes (like IDs) with coordinates?
- Try to figure out how to geocode / map the data:
  - See if Codex can find the right approach.
  - See if you can figure it out yourself.
  - Don’t peek at the instructor’s solution scripts unless you are truly stuck.
- For the scooter/e‑bike example, the goal is to join trips to a street or trail network such as the  
  [PW Street Centerline dataset](https://opendata.minneapolismn.gov/datasets/pw-street-centerline/about) and then map trip counts by segment.

**Task 5.1 (in `Codex_Activity.md`):**
- Describe what was easier and harder about doing this with Codex.
- Where did Codex help, and where did it get in the way?
- What would you do differently next time you had to map a new dataset with AI assistance?

At first, it just plotted the longitude and latitude included in the dataset. When directed to add a base map, it produced several errors, requiring us to install ggmaps and then asking for an api key to run register_stadiamaps(), ignoring instructions to resolve errors - I would have preferred it to try a different way of adding basemaps. It seemed to get in the way when resolving errors; it often did not provide a clear explanation of the solution or other, potentially easier, options. If repeating this exercise, I would give more clear instructions of the packages an processes I wanted it to use. 

# 6. Identifying a research question with Codex

- If you are partner coding, *switch partners*.
- You will now work with Codex to identify and refine a research question (RQ) about your dataset.

## 6.1 Brainstorm candidate questions

- Seed the brainstorming session with some themes or ideas of research questions that interest you.
- Ask Codex to propose several possible research questions.
- Collaborate with Codex to prioritize the questions along the decision dimensions you think are most appropriate.

**Task 6.1 (in `Codex_Activity.md`):**
- List at least two candidate research questions that Codex suggested.
- For each, briefly note what seems interesting and what might be problematic (data, scope, ethics, feasibility).

## 6.2 Refine and choose your question

- Pick one question (it may be a slight modification of a Codex suggestion).
- Ask Codex to critique and refine that question. For example:
  - “Here is my tentative research question: […]. Critique its clarity and feasibility. Propose 1–2 improved versions.”
- Decide on a **final RQ** you will try to answer. This choice is yours, not Codex’s.

**Task 6.2 (in `Codex_Activity.md`):**
- Write down your final research question in one clear sentence.
- In 2–3 sentences, explain why you chose it (and why you rejected other candidates).

# 7. Answering your question with Codex: one-shot vs. plan-first

- You will now use Codex to help answer your chosen research question in two different ways.

## 7.1 One-shot interaction

- In your R or Python environment, ask Codex for a **one-shot** solution to your chosen RQ. For example:
  - “Here is my research question: […]. Write code to answer it in one shot.”
- Run the code (after checking it!) and see what happens:
  - Does the analysis actually answer your question?
  - Does the code run?
  - Are the results interpretable and relevant?

**Task 7.1 (in `Codex_Activity.md`):**
- Briefly describe what analysis Codex performed and what results you obtained.
- What was good or bad about this one-shot attempt (data used, methods, code quality, clarity)?

## 7.2 Plan-first interaction

- Now ask Codex to **plan first, then code** for the *same* research question. For example:
  - “Here is my research question: […]. Help me design a 3–5 step plan to answer it. Don’t write any code yet.”
- Review the plan and edit it so it feels realistic and meaningful to you.
- Then, *step by step*, ask Codex to write code for each step:
  - “Now write code for step 1 only…”
  - Run and debug it.
  - “Now write code for step 2…”
- You may do this in `analysis/codex_activity.Rmd`, `.R`, `.py`, or a notebook.

**Task 7.2 (in `Codex_Activity.md`):**
- Briefly describe the main steps in your final plan and what you implemented for each.
- How did the planned interaction differ from the one-shot attempt?
- Which approach produced better code, and which felt more controllable?

## 7.3 Final reflection

**Task 7.3 (in `Codex_Activity.md`):**
- When would you choose one-shot prompting vs. plan-first prompting for future projects?
- What norms or habits do you want to adopt for working with tools like Codex in your own data science work?

# 8. What to hand in

By the end of the activity, your repo should include at least:

- `Codex_Activity.md` with answers to all **Task** prompts (3.1, 4.1, 5.1, 6.1, 6.2, 7.1, 7.2, 7.3).
- At least one analysis file:
  - R: `analysis/codex_activity.Rmd` or `analysis/codex_activity.R`
  - Python: `analysis/codex_activity.ipynb` or `analysis/codex_activity.py`
- Any additional scripts or notebooks you used, committed in a reasonable state.

Before you’re done:

- Use Codex to help you **commit and push** your work to your forked repo.
- Make sure that your API key is **not** anywhere in the repo.
- Share the URL for the repo (including the markdown and script) on the Moodle activity.
