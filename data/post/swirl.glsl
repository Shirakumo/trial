#section FRAGMENT_SHADER
uniform float radius = 1000;
uniform float angle = 0.8;

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 tex_size = textureSize(previous_pass, 0);
  vec2 center = tex_size/2;
  vec2 tc = (uv * tex_size) - center;
  float dist = length(tc);
  if (dist < radius) {
    float percent = (radius - dist) / radius;
    float theta = percent * percent * angle * 8.0;
    float s = sin(theta);
    float c = cos(theta);
    tc = vec2(dot(tc, vec2(c, -s)), dot(tc, vec2(s, c)));
  }
  return texture(previous_pass, (tc+center) / tex_size);
}
