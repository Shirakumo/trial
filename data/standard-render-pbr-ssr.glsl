#section FRAGMENT_SHADER
#include (trial::trial "ssr.glsl")

uniform sampler2D previous_pass;

void standard_finish@after(){
  vec2 uv = gl_FragCoord.xy / view_size;
  vec3 ref = evaluate_ssr(uv, mat3(view_matrix) * normal);
  vec3 reflection_color = texture(previous_pass, ref.xy).rgb;
  color.rgb = mix(color.rgb, reflection_color, ref.z);
}
