export const POSTURE_WEIGHTS = Object.freeze({
  contender: Object.freeze({ w_win_now: 1.0, w_future: 0.3 }),
  bubble: Object.freeze({ w_win_now: 0.8, w_future: 0.5 }),
  mid: Object.freeze({ w_win_now: 0.6, w_future: 0.7 }),
  rebuild: Object.freeze({ w_win_now: 0.0, w_future: 1.0 }),
});

const VALID_POSTURES = new Set(Object.keys(POSTURE_WEIGHTS));

function normalizeKnownPosture(posture, fallback = "mid") {
  const normalized = String(posture ?? "").trim().toLowerCase();
  return VALID_POSTURES.has(normalized) ? normalized : fallback;
}

export function resolveEffectivePosture(actualPosture, override) {
  const actual = normalizeKnownPosture(actualPosture);
  const requested = String(override ?? "").trim().toLowerCase();

  if (!requested || requested === "auto") {
    return {
      actualPosture: actual,
      effectivePosture: actual,
      overrideApplied: false,
    };
  }

  if (!VALID_POSTURES.has(requested)) {
    throw new Error(
      `Invalid strategy "${requested}". Use auto, contender, bubble, mid, or rebuild.`
    );
  }

  return {
    actualPosture: actual,
    effectivePosture: requested,
    overrideApplied: requested !== actual,
  };
}

export function postureWeights(posture) {
  return POSTURE_WEIGHTS[normalizeKnownPosture(posture)];
}

export function filterTradeTargetsByPosture(rows, effectivePosture) {
  return rows.filter((row) => {
    const rowPosture = row.my_effective_posture ?? row.my_posture;
    return normalizeKnownPosture(rowPosture) === effectivePosture;
  });
}

export function defaultTradeHorizon(effectivePosture, requestedHorizon) {
  const requested = String(requestedHorizon ?? "").trim().toLowerCase();
  if (requested) return requested;
  return effectivePosture === "rebuild" ? "future" : "balanced";
}

export function findTeamPosture(postureRows, teamNeedle) {
  const needle = String(teamNeedle ?? "").trim().toLowerCase();
  if (!needle) return null;

  return (
    postureRows.find(
      (row) =>
        row.billikenTeam &&
        String(row.billikenTeam).toLowerCase().includes(needle)
    ) ?? null
  );
}
