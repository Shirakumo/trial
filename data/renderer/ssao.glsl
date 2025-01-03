#include (trial::trial "depth.glsl")

uniform sampler2D depth_map;
uniform float ssao_radius = 8.0;
uniform float ssao_bias = 5.0;

const int ssao_noise_size = 4;
const vec3[16] ssao_noise = vec3[16]
  (vec3(-0.8007382, 0.8050957, 0.0),
   vec3(0.45105195, 0.9259628, 0.0),
   vec3(0.9571357, -0.016540706, 0.0),
   vec3(0.76381063, -0.059328258, 0.0),
   vec3(0.8387141, 0.30658698, 0.0),
   vec3(-0.8355582, 0.7338513, 0.0),
   vec3(-0.92096627, -0.14211017, 0.0),
   vec3(-0.7973174, -0.03682834, 0.0),
   vec3(-0.65585506, 0.16529632, 0.0),
   vec3(0.3854816, -0.7555276, 0.0),
   vec3(0.7448069, -0.58688796, 0.0),
   vec3(0.89241505, -0.5198358, 0.0),
   vec3(-0.047035515, -0.43614823, 0.0),
   vec3(-0.79357326, -0.6796198, 0.0),
   vec3(-0.8573853, 0.44876075, 0.0),
   vec3(0.17841947, 0.655126, 0.0));

uniform int ssao_kernel_size = 64;
const vec3[64] ssao_kernel = vec3[64]
  (vec3(0.05251223, 0.06853916, 0.050446518),
   vec3(-0.046207886, 0.0068898234, 0.08866429),
   vec3(-0.06774583, 0.010118451, 0.07405858),
   vec3(-0.028726777, -0.051543754, 0.08317111),
   vec3(0.07368136, -0.071300484, 0.014240138),
   vec3(-0.0023180635, 0.07638706, 0.07272175),
   vec3(0.06276085, -0.07234133, 0.049723346),
   vec3(-0.06341586, 0.07831515, 0.045982685),
   vec3(0.025341792, 0.045540202, 0.101460025),
   vec3(-0.09838687, -0.043846086, 0.047685206),
   vec3(0.030222708, 0.086060576, 0.08097836),
   vec3(-0.06831945, 0.0918198, 0.0540909),
   vec3(-0.05972299, 0.04957798, 0.10632236),
   vec3(-0.036792375, -0.12012539, 0.054971714),
   vec3(-0.049943704, -0.06454207, 0.11750721),
   vec3(-0.09654742, 0.03989386, 0.10685941),
   vec3(-0.08653875, 0.08881582, 0.0950624),
   vec3(0.11011674, 0.11056693, 0.04880395),
   vec3(-0.067651875, 0.07170036, 0.13995993),
   vec3(0.14594865, -0.05105331, 0.09082219),
   vec3(0.04843201, -0.07864067, 0.16362418),
   vec3(-0.121613026, 0.0699877, 0.13813534),
   vec3(0.09329818, -0.17752708, 0.0485689),
   vec3(0.13866268, -0.049384978, 0.15840301),
   vec3(0.14301811, -0.13789988, 0.108903676),
   vec3(0.1799917, -0.14368705, 0.057289686),
   vec3(-0.20439665, -0.10927266, 0.08972855),
   vec3(-0.052987527, -0.20443782, 0.15195882),
   vec3(0.21008794, 0.04445926, 0.16737679),
   vec3(-0.13882622, -0.12686312, 0.21386537),
   vec3(-0.16334312, 0.19168898, 0.15884505),
   vec3(-0.17275515, 0.18892553, 0.17686613),
   vec3(-0.26811594, 0.019405285, 0.18265338),
   vec3(-0.20761149, -0.19259918, 0.18685673),
   vec3(0.19308887, -0.24630539, 0.16543616),
   vec3(-0.030406116, 0.28412533, 0.23373294),
   vec3(0.063372865, 0.32525966, 0.19553676),
   vec3(0.28918943, -0.20728081, 0.18452452),
   vec3(0.00521282, 0.24067114, 0.34084764),
   vec3(0.035524674, -0.3207869, 0.2904599),
   vec3(0.28012055, -0.21486329, 0.2815581),
   vec3(-0.28286695, 0.001401008, 0.37454435),
   vec3(0.035348453, 0.31366304, 0.37164155),
   vec3(0.29242593, 0.29804683, 0.2863022),
   vec3(-0.10441211, -0.304444, 0.4152677),
   vec3(0.35674945, 0.23411216, 0.33895102),
   vec3(-0.31028754, 0.35837662, 0.3073217),
   vec3(-0.17502144, 0.3503972, 0.43503365),
   vec3(0.55164725, 0.16573137, 0.18909644),
   vec3(0.20600897, 0.55789757, 0.20036604),
   vec3(0.4803122, 0.05769217, 0.43310922),
   vec3(-0.43723044, -0.46024063, 0.21893415),
   vec3(-0.050753497, 0.34392953, 0.6008059),
   vec3(0.29430047, 0.5355284, 0.37548563),
   vec3(-0.20428, 0.6417421, 0.30839396),
   vec3(-0.48509365, 0.57613885, 0.13218465),
   vec3(-0.52769953, -0.36825603, 0.4566621),
   vec3(0.5739387, -0.4622058, 0.34551346),
   vec3(0.447537, 0.38640594, 0.5954754),
   vec3(0.56634545, 0.3257736, 0.5666757),
   vec3(-0.21421124, -0.37970048, 0.77707785),
   vec3(0.57875293, -0.5140885, 0.49269924),
   vec3(0.15327072, -0.8681177, 0.33941656),
   vec3(0.6880745, 0.18580383, 0.66105866));

float evaluate_ssao(in vec2 uv){
  // get input for SSAO algorithm
  vec3 view_position = depth_world_pos(depth_map, uv, inv_projection_matrix);
  vec3 view_normal = normalize(-cross(dFdy(view_position), dFdx(view_position)));
  ivec2 noise_pos = ivec2(uv * textureSize(depth_map, 0).xy) % ssao_noise_size;
  vec3 random_vec = ssao_noise[noise_pos.x+noise_pos.y*ssao_noise_size];
  // create TBN change-of-basis matrix: from tangent-space to view-space
  vec3 tangent = normalize(random_vec - view_normal * dot(random_vec, view_normal));
  vec3 bitangent = cross(view_normal, tangent);
  mat3 TBN = mat3(tangent, bitangent, view_normal);
  // iterate over the sample kernel and calculate occlusion factor
  float occlusion = 0.0;
  for(int i=0; i < ssao_kernel_size; ++i){
    // get sample position
    vec3 sample_position = TBN * ssao_kernel[i];
    sample_position = view_position + sample_position * ssao_radius;

    // project sample position (to sample texture) (to get position on screen/texture)
    vec4 offset = vec4(sample_position, 1.0);
    offset = projection_matrix * offset;
    offset.xy /= offset.w;
    offset.xy = offset.xy * 0.5 + 0.5;

    // get sample depth
    float sample_depth = depth_world_pos(depth_map, offset.xy, inv_projection_matrix).z;
    float range_check = smoothstep(0.0, 1.0, ssao_radius / abs(view_position.z - sample_depth));
    float depth_diff = sample_depth - sample_position.z;
    occlusion += (depth_diff >= ssao_bias) ? range_check : 0.0;
  }
  occlusion = 1.0 - (occlusion / ssao_kernel_size);
  return pow(occlusion, 2);
}
