#pragma once

#include <fenv.h>

namespace core {

template <uint16_t ExponentWidth, uint16_t SignificandWidth> struct float_traits {
  static constexpr uint16_t exponent_width = ExponentWidth;
  static constexpr uint16_t significand_width = SignificandWidth;
  static constexpr uint16_t sign_width = 1;
  static constexpr bool has_hidden_bit = ((sign_width + exponent_width + significand_width) % 8) != 0;
  static constexpr uint16_t storage_width = sign_width + exponent_width + significand_width + ((has_hidden_bit) ? -1 : 0);
  static constexpr int32_t exponent_bias = (1 << (exponent_width - 1)) + significand_width - 2;
  static constexpr int32_t max_exponent = (1 << (exponent_width - 1)) - significand_width;
  static constexpr int32_t min_exponent = 2 - exponent_bias - significand_width;
  static constexpr int32_t min_normalized_exponent = 1 - exponent_bias;
  using uint_t =
      std::conditional_t<storage_width <= 8, uint8_t,
                         std::conditional_t<storage_width <= 16, uint16_t,
                                            std::conditional_t<storage_width <= 32, uint32_t,
                                                               std::conditional_t<storage_width <= 64, uint64_t, __uint128_t>>>>;
  static constexpr uint_t hidden_bit = has_hidden_bit ? uint_t{1} << (significand_width - 1) : uint_t{0};
  static constexpr uint16_t exponent_shift = storage_width - sign_width - exponent_width;
  static constexpr uint16_t sign_shift = storage_width - sign_width;
  static constexpr uint_t significand_mask = (uint_t{1} << (significand_width + ((has_hidden_bit) ? -1 : 0))) - uint_t{1};
  static constexpr uint_t payload_mask = (uint_t{1} << (significand_width + ((has_hidden_bit) ? -2 : -1))) - uint_t{1};
  static constexpr int32_t non_finite_exponent = static_cast<int32_t>(((uint_t{1} << exponent_width) - uint_t{1}));
  static constexpr uint_t exponent_mask = ((uint_t{1} << exponent_width) - uint_t{1}) << exponent_shift;
  static constexpr uint_t sign_mask = ((uint_t{1} << sign_width) - uint_t{1}) << sign_shift;
  static constexpr uint_t nan_type_mask = uint_t{1} << (significand_width + ((has_hidden_bit) ? -2 : -1));
};

template <typename Float> struct float_convert {
  using traits =
      float_traits<std::bit_width((unsigned int)std::numeric_limits<Float>::max_exponent), std::numeric_limits<Float>::digits>;
  using uint_t = typename traits::uint_t;
  enum class category { finite, quiet_nan, signaling_nan, infinity };

  typedef union {
    Float f;
    unsigned _BitInt(traits::storage_width) b;
  } convert_t;

  static inline uint_t float_to_bits(Float f) {
    convert_t convert = {.f = f};
    return uint_t{convert.b};
  }

  static inline Float bits_to_float(uint_t b) {
    convert_t convert = {.b = b};
    return convert.f;
  }

  struct quadruple {
    category category;
    __uint128_t significand;
    int32_t exponent;
    int16_t sign;
  };

  template<typename Traits = traits>
  static quadruple bits_to_quadruple(typename Traits::uint_t b) {
    quadruple q;

    q.significand = b & Traits::significand_mask;
    q.exponent = static_cast<int32_t>((b & Traits::exponent_mask) >> Traits::exponent_shift);
    q.sign = (b & Traits::sign_mask) ? int32_t{-1} : int32_t{1};

    if (q.exponent != Traits::non_finite_exponent) {
      q.category = category::finite;
      if (q.exponent != 0) {
        // Normal non-zero
        q.significand |= Traits::hidden_bit;
        q.exponent -= Traits::exponent_bias;
      } else if (q.significand != 0) {
        // Subnormal
        int32_t shift = Traits::significand_width - std::bit_width(q.significand);
        q.significand = q.significand << shift;
        q.exponent = 1 - Traits::exponent_bias - shift;
      }
    } else if (q.significand == 0) {
      q.category = category::infinity;
    } else if (q.significand & Traits::nan_type_mask) {
      q.category = category::quiet_nan;
      q.significand &= ~Traits::nan_type_mask;
    } else {
      q.category = category::signaling_nan;
    }

    return q;
  }

  template<typename Traits = traits>
  static typename Traits::uint_t quadruple_to_bits(quadruple q) {
    typename Traits::uint_t b = 0;

    if (q.sign < 0)
      b |= Traits::sign_mask;

    switch (q.category) {
    case category::infinity:
      b |= Traits::exponent_mask;
      break;
    case category::quiet_nan:
      b |= Traits::exponent_mask | Traits::nan_type_mask | (q.significand & Traits::payload_mask);
      break;
    case category::signaling_nan:
      b |= Traits::exponent_mask | ((q.significand == 0) ? uint_t{1} : (q.significand & Traits::payload_mask));
      break;
    default:
      if (q.significand != 0) {
        __uint128_t significand = q.significand;
        int32_t exponent = q.exponent;
        int32_t shift = std::bit_width(significand) - Traits::significand_width;

        // If we don't have enough bits then right shift.
        if (shift < 0) {
          significand = significand << -shift;
          exponent += shift;
          shift = 0;
        }

        // Check for subnormals and set the shift needed to normalize.
        if ((exponent + shift) < Traits::min_normalized_exponent) {
          shift = Traits::min_normalized_exponent - exponent;
        }

        // If we shift away all of the bits that is an underflow.
        if (shift > std::bit_width(significand)) {
          feraiseexcept(FE_UNDERFLOW);
          // Return +/- zero if traps masked.
          return b;
        }

        // Round if we have extra bits.
        if (shift > 0) {
          if (significand & ((1 << shift) - 1))
            feraiseexcept(FE_INEXACT);
          significand = ((significand & (1 << (shift - 1))) ? (significand + (1 << shift)) : significand) >> shift;
          exponent += shift;
        }

        // Check one more time to ensure rounding hasn't increased the width.
        shift = std::max(static_cast<int>(std::bit_width(significand) - Traits::significand_width), 0);
        significand = significand >> shift;
        exponent += shift;

        // Check for overflow.
        if (exponent > Traits::max_exponent) {
          feraiseexcept(FE_OVERFLOW);
          // Return +/- infinity if traps masked.
          return b | Traits::exponent_mask;
        }

        if (std::bit_width(significand) < Traits::significand_width) {
          // Subnormals
          b |= significand & Traits::significand_mask;
        } else {
          // Normal
          b |= (significand & Traits::significand_mask) |
               ((static_cast<uint_t>(exponent + Traits::exponent_bias) << Traits::exponent_shift) & Traits::exponent_mask);
        }
      }
      break;
    }

    return b;
  };

  inline static Float quadruple_to_float(quadruple q) { return bits_to_float(quadruple_to_bits(q)); }

  inline static quadruple float_to_quadruple(Float f) { return bits_to_quadruple(float_to_bits(f)); }
};

} // namespace core
