# UniversalReefID process

## Shapefiles

1.  FWC Oyster Beds in Florida
2.  RCP Managed Areas
3.  SEACAR SampleLocations
4.  "All_Oysters" historical shapefile made up of multiple shapefiles used in the creation of previous iterations of FWC Oyster Beds in Florida shapefile.
    + FWC's "[Index of Oyster Maps in Florida](https://experience.arcgis.com/experience/4b82f5d17a794543bddf889472a763eb)"

## Process
1.  Crosswalk of connected reefs between FWC shapefile and All_Oysters shapefile.
    +  Accounting for internal overlaps within All_Oyster.
    +  Ensures that reefs which overlap with one another within All_Oysters that also overlap within FWC all have the same UniversalReefID.
2.  Accounts for sample locations just outside of MA boundaries but which are connected to reefs within MA.
3.  Median reef size radius is determined from FWC shapefile (currently 14.21m).
4.  Reef matching:
    +  First pass: sample locations are crossed with FWC shapefile, looking for reef matches within 14.21m. Records the OBJECTID of the nearest reef.
        +  \~49% matched at this point.
    +  Second pass: sample locations are then crossed with All_Oysters shapefile to match sample locations that were unable to match in previous pass.
        +  OBJECTID is not available in combined All_Oysters file (unstandardized). An OBJECTID is created and assigned to each unique reef prior to attempting matching.
        +  \~52% matched at this point.
5.  UniversalReefID is then assigned from the crosswalk created in Step 1, with the following hierarchy:
    +  Match for FWC
    +  Match for All_Oysters
6.  Assigning UniversalReefID to values that don't match to a reef and therefore did not receive an ID in the previous step:
    +  Median buffer value is used to match during this part of the process as well.
    +  Unmatched samples will look at nearby (assigned) sample locations and be grouped together within the median buffer distance (14.21m) and assigned the same UniversalReefID where matches are found.
    +  A secondary check is applied which takes these newly assigned "reefs" and looks within median buffer distance (14.21m) for any previously assigned UniversalReefIDs and will inherit the pre-existing UniversalReefID if so.
