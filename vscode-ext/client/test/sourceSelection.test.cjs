const test = require("node:test");
const assert = require("node:assert/strict");
const { selectedNames } = require("../out/sourceSelection.js");

test("maps partial and multi-line selections to complete identifiers", () => {
  const text = "s_1 = a_a s_in\ns_out = a_b s_1";
  assert.deepEqual(selectedNames(text, [{ start: 8, end: 9 }]), ["a_a"]);
  assert.deepEqual(selectedNames(text, [{ start: 0, end: text.length }]), ["s_1", "a_a", "s_in", "s_out", "a_b"]);
});

test("clears empty selections and avoids partial identifiers", () => {
  assert.deepEqual(selectedNames("a_a s_1", [{ start: 2, end: 2 }]), []);
  assert.deepEqual(selectedNames("actor_a a_a", [{ start: 2, end: 4 }]), ["actor_a"]);
  assert.deepEqual(selectedNames("Actor a_a'", [{ start: 0, end: 10 }]), ["a_a'"]);
});
