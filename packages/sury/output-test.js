(i) => {
  let v0;
  try {
    v0 = JSON.parse(i);
  } catch (t) {
    e[0](i);
  }
  if (typeof v0 !== "object" || !v0 || Array.isArray(v0)) {
    e[1](v0);
  }
  let v1 = v0["type"],
    v3 = v0["value"];
  if (v1 !== "info") {
    e[2](v1);
  }
  if (typeof v3 !== "string") {
    e[4](v3);
  }
  let v2;
  try {
    v2 = BigInt(v3);
  } catch (_) {
    e[3](v3);
  }
  return { type: v1, value: v2 };
};
