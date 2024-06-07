#section FRAGMENT_SHADER
#include "ssr.glsl"

out vec4 color;
in vec2 uv;
uniform sampler2D previous_pass;
uniform sampler2D previous_depth;

void main(){
  vec3 ref = evaluate_ssr_(previous_depth, uv);
  vec3 reflection_color = texture(previous_pass, ref.xy).rgb;
  color.rgb = mix(texture(previous_pass, uv).rgb, reflection_color, ref.z);
  //color.rgb = ref;
  color.a = 1.0;
}
