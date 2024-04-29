float random(in float x) {
  x = fract(x * 443.897);
  x *= x + 33.33;
  x *= x + x;
  return fract(x);
}

float random(in vec2 st) {
  vec3 p3  = fract(vec3(st.xyx) * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yzx + 33.33);
  return fract((p3.x + p3.y) * p3.z);
}

float random(in vec3 pos) {
  pos  = fract(pos * vec3(443.897, 441.423, .0973));
  pos += dot(pos, pos.zyx + 31.32);
  return fract((pos.x + pos.y) * pos.z);
}

float random(in vec4 pos) {
  pos = fract(pos * vec4(443.897, 441.423, .0973, .1099));
  pos += dot(pos, pos.wzxy+33.33);
  return fract((pos.x + pos.y) * (pos.z + pos.w));
}

vec2 random2(float p) {
  vec3 p3 = fract(vec3(p) * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yzx + 19.19);
  return fract((p3.xx + p3.yz) * p3.zy);
}

vec2 random2(vec2 p) {
  vec3 p3 = fract(p.xyx * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yzx + 19.19);
  return fract((p3.xx + p3.yz) * p3.zy);
}

vec2 random2(vec3 p3) {
  p3 = fract(p3 * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yzx + 19.19);
  return fract((p3.xx + p3.yz) * p3.zy);
}

vec3 random3(float p) {
  vec3 p3 = fract(vec3(p) * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yzx + 19.19);
  return fract((p3.xxy + p3.yzz) * p3.zyx); 
}

vec3 random3(vec2 p) {
  vec3 p3 = fract(vec3(p.xyx) * vec3(443.897, 441.423, .0973));
  p3 += dot(p3, p3.yxz + 19.19);
  return fract((p3.xxy + p3.yzz) * p3.zyx);
}

vec3 random3(vec3 p) {
  p = fract(p * vec3(443.897, 441.423, .0973));
  p += dot(p, p.yxz + 19.19);
  return fract((p.xxy + p.yzz) * p.zyx);
}

vec4 random4(float p) {
  vec4 p4 = fract(p * vec4(443.897, 441.423, .0973, .1099));
  p4 += dot(p4, p4.wzxy + 19.19);
  return fract((p4.xxyz + p4.yzzw) * p4.zywx);   
}

vec4 random4(vec2 p) {
  vec4 p4 = fract(p.xyxy * vec4(443.897, 441.423, .0973, .1099));
  p4 += dot(p4, p4.wzxy + 19.19);
  return fract((p4.xxyz + p4.yzzw) * p4.zywx);
}

vec4 random4(vec3 p) {
  vec4 p4 = fract(p.xyzx  * vec4(443.897, 441.423, .0973, .1099));
  p4 += dot(p4, p4.wzxy + 19.19);
  return fract((p4.xxyz + p4.yzzw) * p4.zywx);
}

vec4 random4(vec4 p4) {
  p4 = fract(p4  * vec4(443.897, 441.423, .0973, .1099));
  p4 += dot(p4, p4.wzxy + 19.19);
  return fract((p4.xxyz + p4.yzzw) * p4.zywx);
}
