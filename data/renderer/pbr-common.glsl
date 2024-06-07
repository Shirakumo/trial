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

float NDF_ggx(vec3 N, vec3 H, float roughness){
  float a      = roughness*roughness;
  float a2     = a*a;
  float NdotH  = max(dot(N, H), 0.0);
  float NdotH2 = NdotH*NdotH;
  float denom  = (NdotH2 * (a2 - 1.0) + 1.0);
  return a2 / (PI * denom * denom);
}

float G_sggx(float NdotV, float roughness){
  float r = (roughness + 1.0);
  float k = (r*r) / 8.0;
  return NdotV / (NdotV * (1.0 - k) + k);
}

float G_smith(vec3 N, vec3 V, vec3 L, float roughness){
  float ggx1 = G_sggx(max(dot(N, V), 0.0), roughness);
  float ggx2 = G_sggx(max(dot(N, L), 0.0), roughness);
  return ggx1 * ggx2;
}

vec3 F_s(float cos_theta, vec3 F0){
  return F0 + (1.0 - F0) * pow(1.0 - cos_theta, 5.0);
}

vec3 F_s_r(float cosTheta, vec3 F0, float roughness){
    return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}

