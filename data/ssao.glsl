#section FRAGMENT_SHADER
out float color;
in vec2 uv;

uniform sampler2D depth_map;
uniform sampler2D ssao_noise;
uniform sampler2D ssao_kernel;

uniform float radius = 8;
uniform float bias = 5;

vec3 view_position(vec2 coords) {
  float depth = texture(depth_map, coords).r;
  vec4 ndc = vec4(coords.x*2-1, coords.y*2-1, depth*2-1, 1);
  
  vec4 vs_pos = inv_projection_matrix * ndc;
  vs_pos.xyz = vs_pos.xyz / vs_pos.w;
  return vs_pos.xyz;
}

void main(){
  vec2 noise_scale = textureSize(depth_map, 0).xy / 4.0;
  // get input for SSAO algorithm
  vec3 view_position = view_position(position_map, uv);
  vec3 view_normal = cross(dFdy(view_position.xyz), dFdx(view_position.xyz));
  vec3 random_vec = normalize(texture(ssao_noise, uv * noise_scale).xyz);
  // create TBN change-of-basis matrix: from tangent-space to view-space
  vec3 tangent = normalize(random_vec - view_normal * dot(random_vec, view_normal));
  vec3 bitangent = cross(normal, tangent);
  mat3 TBN = mat3(tangent, bitangent, view_normal);
  // iterate over the sample kernel and calculate occlusion factor
  float occlusion = 0.0;
  int kernel_size = textureSize(ssao_kernel, 0).x;
  for(int i = 0; i < kernel_size; ++i){
    // get sample position
    vec3 ssample = TBN * texelFetch(ssao_kernel, i).rgb; // from tangent to view-space
    ssample = view_position + ssample * radius;

    // project sample position (to sample texture) (to get position on screen/texture)
    vec4 offset = vec4(ssample, 1.0);
    offset = projection_matrix * offset; // from view to clip-space
    offset.xyz /= offset.w; // perspective divide
    offset.xy = offset.xy * 0.5 + 0.5; // transform to range 0.0 - 1.0

    // get sample depth
    float sample_depth = view_position(offset.xy).z;
    float range_check = smoothstep(0.0, 1.0, radius / abs(view_position.z - sample_depth));
    occlusion += (sample_depth >= ssample.z + bias ? 1.0 : 0.0) * range_check;
  }
  occlusion = 1.0 - (occlusion / kernel_size);
  color = pow(occlusion, 2);
}
