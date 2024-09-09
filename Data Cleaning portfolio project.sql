
/*
   Cleaning data in SQL queries

*/

Select *
from dbo.NashvilleHousing;

--Standardized the format

Select SaleDate,CONVERT(Date,SaleDate)
from dbo.NashvilleHousing;

update dbo.NashvilleHousing
   set SaleDate=CONVERT(Date,SaleDate);

Alter Table dbo.NashvilleHousing
   ADD SaleDateConverted Date;

update dbo.NashvilleHousing
   set SaleDateConverted=CONVERT(Date,SaleDate);

   select * from dbo.NashvilleHousing

   --Populate PropertyAddress data
 select *
   from dbo.NashvilleHousing
   -- where PropertyAddress is NULL
   order by ParcelID

 select a.ParcelID,a.PropertyAddress,b.ParcelID,b.PropertyAddress,ISNULL(a.PropertyAddress,b.PropertyAddress)
   from dbo.NashvilleHousing a
   join dbo.NashvilleHousing b
   on a.ParcelID=b.ParcelID
   and a.[UniqueID ]<>b.[UniqueID ]
   where a.PropertyAddress is null;


    update a
	Set PropertyAddress=ISNULL(a.PropertyAddress,b.PropertyAddress)
   from dbo.NashvilleHousing a
   join dbo.NashvilleHousing b
   on a.ParcelID=b.ParcelID
   and a.[UniqueID ]<>b.[UniqueID ]
   where a.PropertyAddress is null;

----------------------------------------------------------------------------------------

--Breaking out Address into individual Columns (Address,City,State)

 select PropertyAddress
   from dbo.NashvilleHousing
   -- where PropertyAddress is NULL
  -- order by ParcelID

  Select 
     SUBSTRING(PropertyAddress,1,CHARINDEX(',',PropertyAddress)-1) as Address
	 ,SUBSTRING(PropertyAddress,CHARINDEX(',',PropertyAddress)+1,LEN(PropertyAddress)) as Address
	   from dbo.NashvilleHousing




   Alter Table dbo.NashvilleHousing
   ADD PropertySplitAddress Nvarchar(255);

   update dbo.NashvilleHousing
   set PropertySplitAddress= SUBSTRING(PropertyAddress,1,CHARINDEX(',',PropertyAddress)-1);

   Alter Table dbo.NashvilleHousing
   ADD PropertySplitCity Nvarchar(255);

   update dbo.NashvilleHousing
     set PropertySplitCity= SUBSTRING(PropertyAddress,CHARINDEX(',',PropertyAddress)+1,LEN(PropertyAddress));

	 select *
	   from dbo.NashvilleHousing;

	 select PARSENAME(REPLACE(OwnerAddress,',','.'),3)  -- PARSENAME function work when we have period instead of comma that is why we have done Replace
	 ,PARSENAME(REPLACE(OwnerAddress,',','.'),2)
	 ,PARSENAME(REPLACE(OwnerAddress,',','.'),1)
	 from dbo.NashvilleHousing
	

 Alter Table dbo.NashvilleHousing
   ADD OwnerSplitAddress Nvarchar(255);

   update dbo.NashvilleHousing
   set OwnerSplitAddress= PARSENAME(REPLACE(OwnerAddress,',','.'),3);

    Alter Table dbo.NashvilleHousing
   ADD OwnerSplitCity Nvarchar(255);

   update dbo.NashvilleHousing
   set OwnerSplitCity= PARSENAME(REPLACE(OwnerAddress,',','.'),2);

    Alter Table dbo.NashvilleHousing
   ADD OwnerSplitState Nvarchar(255);

   update dbo.NashvilleHousing
   set OwnerSplitState= PARSENAME(REPLACE(OwnerAddress,',','.'),1);


   Select *
     from dbo.NashvilleHousing

-- Change Y and N to Yes and No in "Solid as Vacant" field

Select distinct(SoldAsVacant),COUNT(SoldAsVacant)
   from dbo.NashvilleHousing
   Group by SoldAsVacant
   order by 2

   Select SoldAsVacant,
      case when SoldAsVacant='N' then 'No'
		   when SoldAsVacant='Y' then 'Yes'
		   else SoldAsVacant
		   END 
   from dbo.NashvilleHousing
  

  update dbo.NashvilleHousing
     set SoldAsVacant=case when SoldAsVacant='N' then 'No'
		   when SoldAsVacant='Y' then 'Yes'
		   else SoldAsVacant
		   END 

		   -----------------------------------------------------------------------------------
	--Remove Duplicates
  with RowNumCTE as (
	Select *,
	  ROW_NUMBER() Over(
	  Partition by ParcelID,
					PropertyAddress,
					SalePrice,
					SaleDate,
					LegalReference
					Order by UniqueID
	  ) RowNumber
	   from dbo.NashvilleHousing
		  -- order by ParcelID
	)
	--Delete
	  Select *
	   from  RowNumCTE
	   where RowNumber>1
	   order by PropertyAddress

------------------------------------------------------------------------------------------------------------

--Delete Unused Columns

select *
   from dbo.NashvilleHousing


   Alter TAble dbo.NashvilleHousing
   drop column OwnerAddress,PropertyAddress,TaxDistrict

    Alter TAble dbo.NashvilleHousing
   drop column SaleDate




