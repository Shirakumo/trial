#section FRAGMENT_SHADER
#include (trial::trial "ssao.glsl")

void standard_init@after(){
  occlusion *= evaluate_ssao(gl_FragCoord.xy / view_size);
}
