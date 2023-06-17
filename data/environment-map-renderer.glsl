#section FRAGMENT_SHADER
out vec4 color;
in vec3 world_position;

uniform sampler2D equirectangular_map;
const vec2 inv_atan = vec2(0.1591, 0.3183);

vec2 sample_spherical_map(vec3 v){
  return vec2(atan(v.z, v.x), asin(-v.y)) * inv_atan + 0.5;
}

void main(){
  vec2 uv = sample_spherical_map(normalize(world_position));
  color = vec4(texture(equirectangular_map, uv).rgb, 1);
}
