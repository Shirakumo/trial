#section FRAGMENT_SHADER
uniform float m_m,a,m,l,c,b;

vec3 tone_map(vec3 color){
  float l0 = ((m_m - m) * l) / a;
  float s0 = m + l0;
  float s1 = m + a*l0;
  float c2 = (a * m_m) / (m_m - s1);
  float cp = -c2 / m_m;

  vec3 w0 = 1.0 - smoothstep(vec3(0), vec3(m), color);
  vec3 w2 = step(vec3(m+l0), color);
  vec3 w1 = vec3(1) - w0 - w2;

  vec3 t = m * pow(color / m, vec3(c)) + b;
  vec3 l = m + a * (color - m);
  vec3 s = m_m - (m_m - s1) * exp(cp * (color - s0));
  return t*w0 + l*w1 + s*w2;
}
