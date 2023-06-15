#section FRAGMENT_SHADER
out vec4 color;
in vec3 world_pos;

uniform int mip_level;
uniform int max_mip_levels;
uniform samplerCube environment_map;
const uint sample_count = 4096u;
const float PI = 3.14159265359;

float radical_inverse_vdc(uint bits) {
  bits = (bits << 16u) | (bits >> 16u);
  bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);
  bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);
  bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);
  bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);
  return float(bits) * 2.3283064365386963e-10; // / 0x100000000
}

vec2 hammersley(uint i, uint N){
  return vec2(float(i)/float(N), radical_inverse_vdc(i));
}

vec3 importance_sample_ggx(vec2 Xi, vec3 N, float roughness){
  float a = roughness*roughness;

  float phi = 2.0 * PI * Xi.x;
  float cos_theta = sqrt((1.0 - Xi.y) / (1.0 + (a*a - 1.0) * Xi.y));
  float sin_theta = sqrt(1.0 - cos_theta*cos_theta);

  // from spherical coordinates to cartesian coordinates
  vec3 H;
  H.x = cos(phi) * sin_theta;
  H.y = sin(phi) * sin_theta;
  H.z = cos_theta;

  // from tangent-space vector to world-space sample vector
  vec3 up = abs(N.z) < 0.999 ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);
  vec3 tangent = normalize(cross(up, N));
  vec3 bitangent = cross(N, tangent);

  vec3 sample_vec = tangent * H.x + bitangent * H.y + N * H.z;
  return normalize(sample_vec);
}

float distribution_ggx(vec3 N, vec3 H, float roughness){
    float a = roughness*roughness;
    float a2 = a*a;
    float n_h = max(dot(N, H), 0.0);
    float n_h2 = n_h*n_h;

    float nom   = a2;
    float denom = (n_h2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return nom / denom;
}

void main(){
  float roughness = mip_level / (max_mip_levels - 1.0);
  float resolution = textureSize(environment_map, 0).x;
  vec3 N = normalize(world_pos);
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
