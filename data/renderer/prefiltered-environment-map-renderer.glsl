#section FRAGMENT_SHADER
#include "pbr-common.glsl"
out vec4 color;
in vec3 world_position;

uniform int mip_level;
uniform int max_mip_levels;
uniform samplerCube environment_map;
const uint sample_count = 4096u;

void main(){
  float roughness = mip_level / (max_mip_levels - 1.0);
  float resolution = textureSize(environment_map, 0).x;
  vec3 N = normalize(world_position);
  vec3 R = N;
  vec3 V = R;

  float total_weight = 0.0;
  vec3 prefiltered = vec3(0.0);
  for(uint i = 0u; i < sample_count; ++i){
    vec2 Xi = hammersley(i, sample_count);
    vec3 H = importance_sample_ggx(Xi, N, roughness);
    vec3 L = normalize(2.0 * dot(V, H) * H - V);

    float n_l = max(dot(N, L), 0.0);
    if(n_l > 0.0){
      // sample from the environment's mip level based on roughness/pdf
      float D = distribution_ggx(N, H, roughness);
      float n_h = max(dot(N, H), 0.0);
      float h_v = max(dot(H, V), 0.0);
      float pdf = D * n_h / (4.0 * h_v) + 0.0001;

      float texel  = 4.0 * PI / (6.0 * resolution * resolution);
      float sampl = 1.0 / (float(sample_count) * pdf + 0.0001);

      float target_mip = roughness == 0.0 ? 0.0 : 0.5 * log2(sampl / texel);
      prefiltered += textureLod(environment_map, L, target_mip).rgb * n_l;
      total_weight += n_l;
    }
  }
  prefiltered = prefiltered / total_weight;
  color = vec4(prefiltered, 1.0);
}
