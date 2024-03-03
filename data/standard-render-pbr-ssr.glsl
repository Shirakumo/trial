#section FRAGMENT_SHADER
#include (trial::trial "ssr.glsl")

void standard_finish@after(){
  vec2 uv = gl_FragCoord.xy / view_size;
  vec4 reflection = evaluate_ssr(uv, normal);
  color = mix(color, reflection.rgb, reflection.a * (1-roughness));
}
