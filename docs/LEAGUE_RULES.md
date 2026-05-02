# Billiken Rotis League Rules (2026)

Quick-reference summary of `Billiken_League_Constitution_2026.pdf` for use by code in this
repo (especially the in-season tooling and trade analysis). Section numbers refer to the
constitution.

## League structure (§1)

- **Teams:** 10 GMs, NL-only.
- **Scoring:** 10 5x5 roto categories.
  - Hitting: R, HR, RBI, SB, AVG
  - Pitching: K (`SO` in our data), W, SV, ERA, WHIP
- **Ranking:** lowest counting stat = 1 pt, highest = N (= 10). For ERA and WHIP, lowest
  ratio = N points (inverted). Ratios computed to 4 decimals. Ties split fractionally.
- **Prize pool (entry fee, traditionally $65/GM):**
  - 1st: 50% of pot
  - 2nd: 30%
  - 3rd: 10%
  - Each category winner outside the top-3 finishers: 1% per category (top-3 finishers
    are not eligible for category prizes).
- **Tiebreaker:** number of categories won, applied to top-3 finish, category prizes, and
  draft order.

## Roster structure (§2)

- **23 active spots per team:** 14 hitters + 9 pitchers.
  - C: 2, 1B: 1, 2B: 1, 3B: 1, SS: 1, CI: 1, MI: 1, OF: 5, Util: 1, P: 9
  - No SP/RP designation among the 9 pitchers.
- **IL:** 8 slots, no cap on total IL'd players (overflow held on bench).
- **Bench:** holds optioned/DFA/SSPD/O players.
- **Minor position:** during the draft only, max 3 players in Minor slots.
- **Position eligibility:** ESPN-managed. 20 games at a position the prior year confers
  eligibility (or primary position if no spot reaches 20). Eligibility can only be gained
  mid-season (10 games at a position), never lost.

## Roster moves (§3)

- **No limit on roster moves; no cost.**
- **FAAB:** $50 per season. Bid window closes 10:01 PM ET nightly. Ties go to lower team
  in the standings (coin flip if also tied). FAAB add must be paired with a corresponding
  drop / IL move / Bench move at least 30 minutes before first MLB game of the day.
- **Trades:**
  - Allowed any time before the pre-draft roster freeze.
  - Can include **next season's draft picks only** (no further future picks).
  - 2-team trades go through ESPN; pick-trades or 3+ team trades emailed/posted.
  - All trades require Commissioner approval.
- **NL/AL trades:** a player traded to AL stays on the GM's roster (still accumulates).
  AL→NL trades enter the player universe and are FAAB-claimable.
- **7-day activation:** IL/Bench/AL-trade returnees must be activated within 7 days or
  they become FAs.
- **Roster freezes:**
  - **Pre-draft:** second Sunday of March, 12:00 noon ET. Non-keepers go into the draft
    pool. No trades after this until the next season.
  - **Pre-playoffs:** 7 days prior to the last day of the MLB regular season. No FAAB,
    no trades.

## Draft (§4)

- **Date:** third Sunday of March, 12:00 noon ET. Conducted via email.
- **Round 1 lottery:** the bottom-7 finishers (ranks 4–10) draw playing cards from a 1993
  All-Stars deck for picks 1–7. Cards are partitioned per finishing rank (more cards →
  worse finish → better odds). Top-3 finishers from prior year get picks 8, 9, 10 of
  round 1 in reverse standings order (3rd → 8, 2nd → 9, 1st → 10). _Note: the
  constitution lists 11th–4th place card buckets, an artifact of past league sizes; in a
  10-team year only 4th–10th get cards._
- **Rounds 2+:** straight reverse standings, last place picks first.
- **Eligibility:** based on MLB.com Transactions status at the moment of the pick. IL'd
  players can't be drafted to active spots; can be drafted to Minor.
- **Supplemental draft:** fills holes left by IL/SSPD/O/minors keepers; happens after
  the main draft.

## Salaries and contracts (§4.4)

This is the section that drives our trade analysis.

- **Source:** salaries set by the NL-only LABR Draft (released first week of March).
- **Default contract on draft:** 2 years guaranteed at the LABR price, then 1 option
  year.
- **Contract column convention used in this repo (`keepers.csv` / `salaries_latest.csv`):**
  - `1` → first year on roster (salary unchanged this year, will be `2` next year).
  - `2` → second year on roster (salary unchanged this year, will be `opt` next year).
  - `opt` → entering the option offseason. The GM can either keep at the same salary
    for one more year, or extend N years at +$5 per added year.
  - A four-digit year (e.g. `2030`) → contract is extended through that year, after
    which the player re-enters the auction.
  - Worked example from the GM's notes: Dylan Crews, $1 contract, was Year 1 then
    Year 2 at $1, then `opt`. Extended through 2030 at +$5 × 4 added years = $21.
- **Trades do not modify contracts.** Salary, contract end-year, and remaining option
  status all travel with the player.
- **Mid-season drop:** contract resets to Year 1 if a different GM picks the player up.
  Original GM must wait 7 days to re-add. New GM inherits the salary at time of drop.
- **AL→NL trades:** the player returns as Year 1 with the AL LABR salary.
- **Drop penalty for extended contracts:** if a GM drops mid-season or fails to keep a
  player who is on a contract extension, that GM's next-draft cap is reduced by **$5
  per year remaining** on the contract. Career-ending injury / death and trades to AL
  are exempt.

## Keeper allotments (§4.2)

- 1st place GM: **10**
- 2nd place GM: **11**
- 3rd place GM: **12**
- Everyone else: **15**
- **Minimum keepers per GM: 7.**
- Keepers entered into ESPN by the pre-draft roster freeze (one week before draft).

## Salary cap (§4.5)

- $270 per GM for the draft. Spending the cap before filling 23 spots forfeits the
  remaining picks; the supplemental draft fills the rest. Supplemental picks retain
  their salary for future seasons.

## Implications for the trade-analysis tooling

- **Multi-year value math:** salaries are flat for years 1–2, optionally +$5/yr for any
  extension after the option season. Salary inflation only kicks in at the opt year.
- **"Expiring" ≠ contract_end == current year.** A `year2` player has `contract_end`
  equal to the current season (since year 2 is the last guaranteed year), but they are
  NOT expiring after this season — they roll into the option year next offseason and
  the GM can extend them or keep them at the same salary for one more year. The only
  truly expiring contracts are:
    * `contract_status == "opt"` — currently in the option year, becomes a FA after
      this season unless the GM had already extended them (in which case the code
      would already be a year, not `opt`).
    * `contract_status == "extended"` AND `contract_end == current_year` — the GM
      explicitly extended only through this year.
  This matters for trade analysis because contender teams are specifically hunting for
  these truly-expiring contracts, not for `year2` players who still have keeper years
  ahead of them.
- **Drop penalties** make long-term contracts a real liability if you don't actually want
  to keep the player; this should reduce the perceived trade value of a long deal that
  the receiving team would have to keep or eat.
- **Pick-trade window** is one season ahead only — the matchmaker should never propose
  picks two-plus years out.
- **Top-3 keeper cap (10/11/12)** forces those teams to shed 3–5 keepers worth of
  current-roster value next offseason. That makes them more receptive to trading
  borderline keepers, especially extended contracts that would otherwise produce drop
  penalties.
- **Round-1 lottery** means a contender's "next-year first" is a probabilistic asset:
  a top-3 finisher's pick is locked at slot 8/9/10, but anyone in the bottom 7 has a
  non-trivial chance at #1 overall. The pick-value model should respect this.
