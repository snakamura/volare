#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    float latitude;
    float longitude;
    float airPressure;
    float surfaceAirPressure;
    float eastwardWind;
    float northwardWind;
    float airTemperature;
    int relativeHumidity;
    float rainfallRate;
    int upperCloudiness;
    int midCloudiness;
    int lowCloudiness;
    int cloudAmount;
} Surface;

typedef struct {
    float latitude;
    float longitude;
    float airePressure;
    float geopotentialHeight;
    float lagrangianTendencyOfAirPressure;
    float eastwardWind;
    float northwardWind;
    float airTemperature;
    int relativeHumidity;
} Pressure;

size_t surface(const char* path,
               const float nwLatitude,
               const float nwLongitude,
               const float seLatitude,
               const float seLongitude,
               const int time,
               Surface *surfaces,
               const size_t surfaceCount);

#ifdef __cplusplus
}
#endif
