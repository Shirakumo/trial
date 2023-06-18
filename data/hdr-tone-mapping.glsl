#section FRAGMENT_SHADER
uniform float inv_gamma = 1.0/2.2;

float color_luminance(vec3 color){
  return dot(color, vec3(0.299, 0.587, 0.114));
}

vec3 tone_map(vec3 color);

void main(){
  maybe_call_next_method();
  vec3 mapped = tone_map(color.xyz);
  mapped = clamp(mapped, vec3(0), vec3(1));
  color.xyz = pow(mapped.xyz, vec3(inv_gamma));
}

#ifdef TONE_MAP_HABLE
#include (trial::trial "tone-map/hable.glsl")
#elif TONE_MAP_HILL_ACES
#include (trial::trial "tone-map/hill-aces.glsl")
#elif TONE_MAP_NARKOWICZ_ACES
#include (trial::trial "tone-map/narkowicz-aces.glsl")
#elif TONE_MAP_REINHARD_EXTENDED
#include (trial::trial "tone-map/reinhard-extended.glsl")
#elif TONE_MAP_REINHARD
#include (trial::trial "tone-map/reinhard.glsl")
#elif TONE_MAP_SCHLICK
#include (trial::trial "tone-map/schlick.glsl")
#elif TONE_MAP_TUMBLIN_RUSHMEIER
#include (trial::trial "tone-map/tumblin-rushmeier.glsl")
#elif TONE_MAP_UCHIMURA
#include (trial::trial "tone-map/uchimura.glsl")
#elif TONE_MAP_WARD
#include (trial::trial "tone-map/ward.glsl")
#endif
