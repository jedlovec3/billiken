import test from "node:test";
import assert from "node:assert/strict";

import {
  defaultTradeHorizon,
  findTeamPosture,
  filterTradeTargetsByPosture,
  POSTURE_WEIGHTS,
  postureWeights,
  resolveEffectivePosture,
} from "../lib/trade_posture.js";

test("auto and omitted overrides preserve the actual posture", () => {
  assert.deepEqual(resolveEffectivePosture("mid"), {
    actualPosture: "mid",
    effectivePosture: "mid",
    overrideApplied: false,
  });
  assert.deepEqual(resolveEffectivePosture("mid", "auto"), {
    actualPosture: "mid",
    effectivePosture: "mid",
    overrideApplied: false,
  });
});

test("an explicit strategy overrides the actual posture case-insensitively", () => {
  assert.deepEqual(resolveEffectivePosture("mid", " Rebuild "), {
    actualPosture: "mid",
    effectivePosture: "rebuild",
    overrideApplied: true,
  });
  assert.deepEqual(postureWeights("rebuild"), {
    w_win_now: 0,
    w_future: 1,
  });
});

test("unknown actual postures fall back to mid when no override is selected", () => {
  assert.deepEqual(resolveEffectivePosture("unexpected"), {
    actualPosture: "mid",
    effectivePosture: "mid",
    overrideApplied: false,
  });
  assert.deepEqual(postureWeights("unexpected"), POSTURE_WEIGHTS.mid);
});

test("unknown explicit strategies are rejected", () => {
  assert.throws(
    () => resolveEffectivePosture("mid", "all-in"),
    /Invalid strategy "all-in"/
  );
});

test("suggestions are filtered to the selected effective strategy", () => {
  const rows = [
    { id: 1, my_effective_posture: "mid", my_posture: "mid" },
    { id: 2, my_effective_posture: "rebuild", my_posture: "rebuild" },
    { id: 3, my_posture: "rebuild" },
  ];

  assert.deepEqual(
    filterTradeTargetsByPosture(rows, "rebuild").map((row) => row.id),
    [2, 3]
  );
});

test("rebuild suggestions default to future ranking", () => {
  assert.equal(defaultTradeHorizon("rebuild"), "future");
  assert.equal(defaultTradeHorizon("mid"), "balanced");
  assert.equal(defaultTradeHorizon("rebuild", "win_now"), "win_now");
});

test("actual posture resolves independently of generated trade rows", () => {
  const postureRows = [
    { billikenTeam: "BLUE SOCKS", posture: "rebuild" },
    { billikenTeam: "HOOSIERS", posture: "contender" },
  ];

  assert.deepEqual(findTeamPosture(postureRows, "blue socks"), {
    billikenTeam: "BLUE SOCKS",
    posture: "rebuild",
  });
  assert.equal(findTeamPosture(postureRows, "missing"), null);
});
