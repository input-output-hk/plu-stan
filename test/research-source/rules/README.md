# Rules listing

This folder contains the rules to be implemented, organized into separate files according to the tags that were found across the audits.
Each rule includes:

- The category they belong to (e.g., Security, Performance, Code Quality)
- A description of its intent (what it aims to detect)
- The detection logic (how it detects it)
- Examples of valid and invalid code. Valid cases would be examples that are correctly written and should have no error/warning detection by the static analyzer. Invalid cases would be examples that have an issue and should be detected

## How to add new rules

Each rule must follow the common structure described above.

To add a new rule, create a new file inside this `rules` folder. The file should be named after the rule it describes, using a clear and descriptive name.

Each rule file should include:

1. **Title and category**
   The rule name should appear at the top of the file, followed by its category (or categories) in brackets (e.g., `[Security]`, `[Performance]`, `[Code Quality]`).

2. **Description**
   A brief explanation of the rule’s intent: what the rule aims to detect and why it is relevant. This section may also mention the potential consequences of not addressing the issue.

3. **Detection logic**
   A high level description of how the rule can be detected. This is typically written in pseudocode, using placeholders (e.g., `<expression>`, `<input>`) to indicate the relevant code patterns.
   The detection logic does not need to be fully exhaustive, it may describe common or representative cases that trigger the rule.

4. **Examples**
   Provide examples of both **valid** and **invalid** code:
   - Valid examples illustrate code that follows the rule and should not trigger any warning or error.
   - Invalid examples illustrate code that violates the rule and should be flagged by the static analyzer.

   Examples do not always need to be complete implementations. In many cases, they focus only on the critical code fragment that demonstrates the issue being addressed.
