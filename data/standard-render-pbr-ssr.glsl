#section FRAGMENT_SHADER
#include (trial::trial "ssr.glsl")

uniform sampler2D previous_pass;

void standard_finish@after(){
  vec2 uv = gl_FragCoord.xy / view_size;
  vec3 ref = evaluate_ssr(uv, mat3(view_matrix) * normal);
  //vec3 reflection_color = pow(texture(previous_pass, ref.xy).rgb, vec3(gamma));
  //color.rgb = mix(color.rgb, reflection_color, 1.0);
  //color.rgb = texture(previous_pass, uv).rgb;
  color.rgb = vec3(ref);
}
