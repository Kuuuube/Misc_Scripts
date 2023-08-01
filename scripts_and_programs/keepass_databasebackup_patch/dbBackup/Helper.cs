// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Helper.cs" company="">
//   
// </copyright>
// <summary>
//   The helper.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace DataBaseBackup
{
    using System.IO;

    using KeePass.Plugins;

    /// <summary>
    /// The helper.
    /// </summary>
    public static class Helper
    {
        #region Public Methods and Operators

        /// <summary>
        /// The get log file name.
        /// </summary>
        /// <param name="host">
        /// The host.
        /// </param>
        /// <returns>
        /// The <see cref="string"/>.
        /// </returns>
        public static string GetLogFileName(IPluginHost host)
        {
            string SourceFileName = string.Empty;
            if (host.Database.IOConnectionInfo.IsLocalFile())
            {
                // local file
                var f = new FileInfo(host.Database.IOConnectionInfo.Path);
                SourceFileName = f.Name;

                f = null;
            }
            else
            {
                // remote file
                SourceFileName = host.Database.IOConnectionInfo.Path;

                int lastPosBack = SourceFileName.LastIndexOf("/");
                if (lastPosBack > 0 && lastPosBack < SourceFileName.Length)
                {
                    SourceFileName = SourceFileName.Substring(lastPosBack + 1);
                }

                int lastPosSlash = SourceFileName.LastIndexOf(@"\");
                if (lastPosSlash > 0 && lastPosSlash < SourceFileName.Length)
                {
                    SourceFileName = SourceFileName.Substring(lastPosSlash + 1);
                }
            }

            return SourceFileName + "_log";
        }

        /// <summary>
        /// The get source file name.
        /// </summary>
        /// <param name="host">
        /// The host.
        /// </param>
        /// <returns>
        /// The <see cref="string"/>.
        /// </returns>
        public static string GetSourceFileName(IPluginHost host)
        {
            string SourceFileName = string.Empty;
            if (host.Database.IOConnectionInfo.IsLocalFile())
            {
                // local file
                var f = new FileInfo(host.Database.IOConnectionInfo.Path);
                SourceFileName = f.Name;

                // remove file extension
                if (!string.IsNullOrEmpty(f.Extension))
                {
                    SourceFileName = SourceFileName.Substring(0, SourceFileName.Length - f.Extension.Length);
                }

                f = null;
            }
            else
            {
                // remote file
                SourceFileName = host.Database.IOConnectionInfo.Path;

                int lastPosBack = SourceFileName.LastIndexOf("/");
                if (lastPosBack > 0 && lastPosBack < SourceFileName.Length)
                {
                    SourceFileName = SourceFileName.Substring(lastPosBack + 1);
                }

                int lastPosSlash = SourceFileName.LastIndexOf(@"\");
                if (lastPosSlash > 0 && lastPosSlash < SourceFileName.Length)
                {
                    SourceFileName = SourceFileName.Substring(lastPosSlash + 1);
                }

                // remove file extension
                int lastPoitDot = SourceFileName.LastIndexOf(".");
                int lenghtWithoutDot = SourceFileName.Length - lastPoitDot;
                if (lenghtWithoutDot == 3 || lenghtWithoutDot == 4 || lenghtWithoutDot == 5)
                {
                    SourceFileName = SourceFileName.Substring(0, lastPoitDot);
                }
            }

            return SourceFileName;
        }

        #endregion
    }
}