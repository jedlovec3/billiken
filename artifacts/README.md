# Investigation and delivery artifacts

Store one-off SQL, dated evidence, and investigation notes under investigations/<date>-<topic>/. An imported-YYYY-MM-DD folder records the migration date, not the execution date of the query. Never execute archived SQL automatically; some historical files describe mutations or rollbacks.

Preserve source and observation dates. Distill reusable conclusions into docs/ and link back to the investigation. Keep raw customer exports and machine-generated data in ignored local-only/ or existing ignored outputs/ folders. Reusable query tools may stay in established query directories.

The first import is selective: text context and reviewed SQL are preserved; generated decks, PDFs, videos, raw CSV/TSV exports, and database indexes remain local.
