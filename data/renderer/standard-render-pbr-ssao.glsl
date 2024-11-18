#section FRAGMENT_SHADER
#include "ssao.glsl"

float ssao_strength = 1.0;

void standard_init@after(){
  occlusion *= mix(1.0, evaluate_ssao(gl_FragCoord.xy / view_size), ssao_strength);
}
