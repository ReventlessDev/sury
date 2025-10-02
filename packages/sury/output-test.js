(i) => {
  let v2 = new Array(i.length);
  for (let v0 = 0; v0 < i.length; ++v0) {
    try {
      let v1 = i[v0];
      if (v1 === void 0) {
        v1 = null;
      }
      v2[v0] = v1;
    } catch (v3) {
      if (v3 && v3.s === s) {
        v3.path = "[\"'+v0+'\"]" + v3.path;
      }
      throw v3;
    }
  }
  return v2;
};

(i) => {
  let v4 = new Array(i.length);
  for (let v0 = 0; v0 < i.length; ++v0) {
    let v3;
    try {
      let v2 = i[v0];
      if (v2 === void 0) {
        v2 = null;
      }
      v3 = v2;
    } catch (v1) {
      if (v1 && v1.s === s) {
        v1.path = "" + "[\"'+v0+'\"]" + v1.path;
      }
      throw v1;
    }
    v4[v0] = v3;
  }
  return v4;
};
