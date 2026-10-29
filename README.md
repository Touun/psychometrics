# Psycholinguistic Analysis Dashboard (Prototype)

A rapid-prototype Shiny dashboard that performs psycholinguistic analysis using Grok (xAI) on Twitter handles or raw text corpora. It maps extracted evidence against the Big Five NEO-PI-R personality traits, assesses entrepreneurial traits, and flags behavioral discrepancies.

> **Note on Running This Repo:**  
> This repository is published strictly as a **code-showcase and proof of concept** alongside the live demo. The deployment `manifest.json` tracks local environment files that are intentionally omitted from version control for security. Cloning and running this out of the box will not work without manual environment setup and a valid xAI API key.

The live demo can be accessed on Posit RShiny platform here: https://tachiaoi.shinyapps.io/demo/
*(Note: The xAI API key is currently unprovisioned with credits, so running a live query will return an error.)*

---
## Core Intuition behind this prototype

Recent AI and computational linguistics research suggests that languages share a universal geometric shape in high-dimensional latent space: regardless of the language or architecture, models converge on the same underlying semantic manifold.

By leveraging these hyperdimensional representations, LLMs do not simply parse text—they navigate an embedded topology where word choice, sentence structure, and conversational flow correlate directly with psychological traits, affective states, and cognitive patterns. This prompt operationalizes that geometry by framing the model as a psycholinguist, projecting unstructured dialogue transcripts onto the 30 facets of the Big Five (NEO-PI-R), entrepreneurial markers, and diagnostic behavioral signals.

---

## Why Shiny?

This prototype was built with Shiny because my technical background is initially rooted in R, and I wanted to leverage Posit's seamless, one-click deployment workflow. It allowed me to move from prompt engineering to a working, interactive interface without getting bogged down in front-end frameworks or disparate DevOps plumbing.

---

## Core Concepts & Architectural Goals

This codebase was developed to test feasibility under specific prototype constraints:

* **Speed of Execution & Feasibility:** Focus on rapid validation of the core analysis pipeline rather than building production-grade enterprise software.
* **API Exploration:** Contains initial traces of what would eventually become a standalone API service (e.g., Plumber integration traces). The current code is intentionally monolithic and exploratory.
* **Strict Prompt Engineering & Structured Outputs:** Pushing the boundaries of complex system prompting (`system_prompt.txt`) to extract 30 NEO-PI-R subfacets, entrepreneurship markers, and red flags while enforcing a strict JSON schema via API parameters (`response_format = list(type = "json_object")`).
* **Session Persistence:** A lightweight embedded SQLite layer (`RSQLite` / `DBI`) for caching and retrieving previously run profiles dynamically.

---

## Technical Stack

* **Framework:** [R Shiny](https://shiny.posit.co/)
* **HTTP Client:** [`httr2`](https://httr2.r-lib.org/) for asynchronous-style HTTP handling and timeouts
* **Persistence:** SQLite via `DBI` / `RSQLite`
* **UI Utilities:** `shinyjs`, `bslib`
* **LLM Engine:** xAI Grok API (`grok-4-latest` with live X search capabilities)

---

## Environment & Secrets (For Reference)

The application expects credentials to be passed via environment variables (typically defined in a local `.Renviron` file):

```env
# .Renviron
XAI_API_KEY="your-xai-api-key"
