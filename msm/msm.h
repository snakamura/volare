#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _surface_item {
    float latitude;
    float longitude;
    float air_pressure;
    float surface_air_pressure;
    float eastward_wind;
    float northward_wind;
    float air_temperature;
    int relative_humidity;
    float rainfall_rate;
    int upper_cloudiness;
    int mid_cloudiness;
    int low_cloudiness;
    int cloud_amount;
} surface_item;

typedef struct _barometric_item {
    float latitude;
    float longitude;
    int air_pressure;
    float geopotential_height;
    float lagrangian_tendency_of_air_pressure;
    float eastward_wind;
    float northward_wind;
    float air_temperature;
    int relative_humidity;
} barometric_item;

size_t get_surface_items(const char* path,
                         const float nwLatitude,
                         const float nwLongitude,
                         const float seLatitude,
                         const float seLongitude,
                         const int time,
                         surface_item* items,
                         const size_t itemCount);

size_t get_barometric_items(const char* path,
                            const float nwLatitude,
                            const float nwLongitude,
                            const float seLatitude,
                            const float seLongitude,
                            const int time,
                            barometric_item* items,
                            const size_t itemCount);

#ifdef __cplusplus
}
#endif
