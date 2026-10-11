# billiken working instructions


## Durable context and repository maintenance

- Read README.md and docs/index.md first; use the smallest relevant source. Preserve existing project-specific rules.
- Store evergreen, reusable knowledge in docs/ or the existing kb/knowledge-bank structure. Include source links, validation dates, and evidence labels; distinguish observations, drafts, and confirmed decisions.
- Store one-off SQL, dated investigations, and review snapshots under artifacts/investigations/<date>-<topic>/, separate from evergreen guidance. Promote conclusions into docs/ after verification. Reusable query tools may remain in their established query directories.
- Keep credentials, raw customer exports, personal data, dependency folders, temporary renders, and machine indexes out of shared context. Private repositories still require credential hygiene.
- At the end of substantive work, update relevant context and navigation, review selected changes, commit, fetch, and push to the owned repository's current branch when authorized by the task. Never use blanket git add, force push, discard unrelated work, or rewrite shared history.
- In organizational repositories used by others, make narrowly scoped branch/PR changes only; do not restructure or push shared branches aggressively.
- Linked company systems remain authoritative. Repository snapshots describe their stated validation date, not guaranteed current behavior.
