// --------------------------------------------------------------------------------------------------------------------
// <copyright file="AppConfig.cs" company="">
//   
// </copyright>
// <summary>
//   The app config.
// </summary>
// --------------------------------------------------------------------------------------------------------------------
namespace DataBaseBackup
{
    using System.Collections.Specialized;

    using KeePass.App.Configuration;

    /// <summary>
    ///     The app config.
    /// </summary>
    public class AppConfig
    {
        AceCustomConfig aceCustomConfig;

        public AppConfig(AceCustomConfig appConfig)
        {
            aceCustomConfig = appConfig;
        }

        #region Public Properties

        /// <summary>
        ///     Gets or sets a value indicating whether auto backup.
        /// </summary>
        public bool AutoBackup
        {
            get
            {
                bool returnValue = true;
                returnValue = this.aceCustomConfig.GetBool("DataBaseBackup_AutoBackup", true);
                return returnValue;
            }

            set
            {
                this.aceCustomConfig.SetBool("DataBaseBackup_AutoBackup", value);
            }
        }

        /// <summary>
        ///     Gets or sets the date format.
        /// </summary>
        public string DateFormat
        {
            get
            {
                string returnValue = string.Empty;
                returnValue = this.aceCustomConfig.GetString("DataBaseBackup_DateFormat", "dd-MMMM-yyyy-hh-mm-ss");
                return returnValue;
            }

            set
            {
                this.aceCustomConfig.SetString("DataBaseBackup_DateFormat", value);
            }
        }

        /// <summary>
        ///     Gets or sets the histo folder.
        /// </summary>
        public StringCollection HistoFolder
        {
            get
            {
                var returnValue = new StringCollection();

                long itemCount = this.aceCustomConfig.GetLong("DataBaseBackup_HistoFolder_Qty", 0);
               
                for (int i = 0; i < itemCount; i++)
                {
                    returnValue.Add(this.aceCustomConfig.GetString("DataBaseBackup_HistoFolder_" + i.ToString(), string.Empty));
                }

                return returnValue;
            }

            set
            {
                this.aceCustomConfig.SetLong("DataBaseBackup_HistoFolder_Qty", value.Count);
                for (int i = 0; i < value.Count; i++)
                {
                    this.aceCustomConfig.SetString("DataBaseBackup_HistoFolder_" + i.ToString(), value[i]);
                }
            }
        }

        /// <summary>
        ///     Gets or sets the histo qty.
        /// </summary>
        public ulong HistoQty
        {
            get
            {
                return this.aceCustomConfig.GetULong("DataBaseBackup_HistoQty", 5);
            }

            set
            {
                this.aceCustomConfig.SetULong("DataBaseBackup_HistoQty", value);
            }
        }

        #endregion
    }
}