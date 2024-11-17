#section FRAGMENT_SHADER
uniform float intensity = 1.0;
uniform vec2 dir = vec2(1.0, 0.0);
uniform float weight[5] = float[] (0.227027, 0.1945946, 0.1216216, 0.054054, 0.016216);

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 tex_offset = 1.0 / textureSize(previous_pass, 0);
  vec3 result = texture(previous_pass, uv).rgb * weight[0];
  for(int i = 1; i < 5; ++i){
    result += texture(previous_pass, uv + tex_offset*dir*i*intensity).rgb * weight[i];
    result += texture(previous_pass, uv - tex_offset*dir*i*intensity).rgb * weight[i];
  }
  return vec4(result, 1.0);
}
