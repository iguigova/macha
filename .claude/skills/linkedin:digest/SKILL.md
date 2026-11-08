---
name: linkedin:digest
description: Read the user's LinkedIn feed, distil what matters into a one-page gist PDF, and email it to them with its title as the subject
argument-hint: "(no arguments: it reads the feed as it is now)"
allowed-tools: mcp__playwright__browser_navigate, mcp__playwright__browser_snapshot, mcp__playwright__browser_evaluate, mcp__playwright__browser_close, Read, Write, Bash(date *), Bash(~/.claude/scripts/gist.py *), Bash(pdftoppm *), Bash(.claude/scripts/gist_mail.py *)
---

Read the LinkedIn feed and mail its gist. `<date>` is `date +%F`, and
every file goes in `.claude/linkedin_digest/`. Every run ends with
`browser_close`, whichever step it stops at.

1. **Open the feed.** `browser_navigate` to `https://www.linkedin.com/feed/`.
   A login, authwall or checkpoint page means the saved login has expired:
   stop, and say the user runs
   `! ~/.claude/scripts/playwright_login.sh https://www.linkedin.com/login`.
2. **Read it.** `browser_evaluate` with the function in
   `.claude/skills/linkedin:digest/feed_read.js`, saved with `filename`
   `.claude/linkedin_digest/<date>-feed.json`. It scrolls the feed and
   returns each post's text and link, comments included in the text.
   An `n` of 0 is a failed read: stop and say so.
3. **Distil it.** Read the posts and `jobs/profile/profile.txt`. Comments
   are ignored; a post marked Promoted is an ad. Write
   `.claude/linkedin_digest/<date>.json` by `~/.claude/skills/gist/SKILL.md`
   (Write the content), with these parts fixed:
   - **subtitle** — `<first name>'s LinkedIn feed, read <date> at <time>:
     <n> unique items, comments ignored`.
   - **metrics** — the posts' own headline numbers, and the count of real
     posts and of ads among the `n` items.
   - **columns** — What mattered, What it means for you (by the profile:
     job leads, interview material, the user's own work), and Noise to
     discard.
   - **note** — whose figures they are, and that the counts come from one
     scroll, so LinkedIn's ranking chose what showed.
4. **Render and look.**
   ```bash
   ~/.claude/scripts/gist.py .claude/linkedin_digest/<date>.json .claude/linkedin_digest/<date>_gist.pdf
   pdftoppm -png -r 110 -singlefile .claude/linkedin_digest/<date>_gist.pdf .claude/linkedin_digest/<date>_gist
   ```
   An error names the part that does not fit: cut words from it and render
   again. Read the PNG and check that no label crowds the one beside it.
5. **Send.**
   ```bash
   .claude/scripts/gist_mail.py .claude/linkedin_digest/<date>.json \
     .claude/linkedin_digest/<date>_gist.pdf --stamp .claude/linkedin_digest.last
   ```
   A refusal is reported as it prints, and nothing is retried.
