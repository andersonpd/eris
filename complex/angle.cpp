 // Convert a value in combined axis-angle representation to a quaternion.
// The value angle_axis is a triple whose norm is an angle in radians,
// and whose direction is aligned with the axis of rotation,
// and quaternion is a 4-tuple that will contain the resulting quaternion.
// The implementation may be used with auto-differentiation up to the first
// derivative, higher derivatives may have unexpected results near the origin.

template<typename T>
inline void AngleAxisToQuaternion(const T* angle_axis, T* quaternion) {
  const T &a0 = angle_axis[0];
  const T &a1 = angle_axis[1];
  const T &a2 = angle_axis[2];
  const T theta_squared = a0 * a0 + a1 * a1 + a2 * a2;

  // For points not at the origin, the full conversion is numerically stable.
  if (theta_squared > T(0.0)) {
    const T theta = sqrt(theta_squared);
    const T half_theta = theta * T(0.5);
    const T k = sin(half_theta) / theta;
    quaternion[0] = cos(half_theta);
    quaternion[1] = a0 * k;
    quaternion[2] = a1 * k;
    quaternion[3] = a2 * k;
  } else {
    // At the origin, sqrt() will produce NaN in the derivative since
    // the argument is zero.  By approximating with a Taylor series,
    // and truncating at one term, the value and first derivatives will be
    // computed correctly when Jets are used.
    const T k(0.5);
    quaternion[0] = T(1.0);
    quaternion[1] = a0 * k;
    quaternion[2] = a1 * k;
    quaternion[3] = a2 * k;
  }
}

// Convert a quaternion to the equivalent combined axis-angle representation.
// The value quaternion must be a unit quaternion - it is not normalized first,
// and angle_axis will be filled with a value whose norm is the angle of
// rotation in radians, and whose direction is the axis of rotation.
// The implemention may be used with auto-differentiation up to the first
// derivative, higher derivatives may have unexpected results near the origin.

template<typename T>
inline void QuaternionToAngleAxis(const T* quaternion, T* angle_axis) {
  const T &q1 = quaternion[1];
  const T &q2 = quaternion[2];
  const T &q3 = quaternion[3];
  const T sin_squared = q1 * q1 + q2 * q2 + q3 * q3;

  // For quaternions representing non-zero rotation, the conversion
  // is numerically stable.
  if (sin_squared > T(0.0)) {
    const T sin_theta = sqrt(sin_squared);
    const T k = T(2.0) * atan2(sin_theta, quaternion[0]) / sin_theta;
    angle_axis[0] = q1 * k;
    angle_axis[1] = q2 * k;
    angle_axis[2] = q3 * k;
  } else {
    // For zero rotation, sqrt() will produce NaN in the derivative since
    // the argument is zero.  By approximating with a Taylor series,
    // and truncating at one term, the value and first derivatives will be
    // computed correctly when Jets are used.
    const T k(2.0);
    angle_axis[0] = q1 * k;
    angle_axis[1] = q2 * k;
    angle_axis[2] = q3 * k;
  }
}