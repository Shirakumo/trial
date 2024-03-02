#section FRAGMENT_SHADER
#include (trial::trial "ssao.glsl")

void standard_init@after(){
  vec2 uv = gl_FragCoord.xy / view_size;
  occlusion *= evaluate_ssao(uv);
}
